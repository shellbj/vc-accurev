;;; vc-accurev.el --- VC backend for the Accurev version-control system

;; Copyright (C) 2008 Bryan Shell

;; Author:      Bryan Shell
;; Maintainer:  Bryan Shell

;;; Commentary:

;;; Bugs:

;;; Code:

(eval-when-compile
  (require 'vc)
  (require 'cl)
  (require 'eieio))

(require 'xml)

(add-to-list 'vc-handled-backends 'Accurev)

;; Clear the vc cache to force vc-call to check again and discover new
;; fuctions when we reload this file.
(put 'Accurev 'vc-functions nil)

;;;
;;; Customization options
;;;

(defgroup vc-accurev nil
  "VC Accurev backen."
  :version "22.2"
  :group 'vc)

(defcustom vc-accurev-program "accurev"
  "Name of the accurev command (excluding any arguments.)"
  :group 'vc-accurev
  :type 'string)

(defcustom vc-accurev-global-switches nil
  "String/list of strings specifying extra switches for accurev any command under VC."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-accurev)

;; BACKEND PROPERTIES
(defun vc-accurev-revision-granularity ()
  "Takes no arguments.  Returns either 'file or 'repository.  Backends
that return 'file have per-file revision numbering; backends
that return 'repository have per-repository revision numbering,
so a revision level implicitly identifies a changeset."
  'file)

;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-accurev-registered (file)
;;;###autoload   (if (executable-find "accurev")
;;;###autoload       (progn
;;;###autoload 	       (load "vc-accurev")
;;;###autoload 	       (vc-accurev-registered file))))

(defun vc-accurev-registered (file)
  "Return non-nil if FILE is registered with Accurev."
  (let ((dir (vc-accurev-root file)))
    (when dir
      (let ((state (vc-accurev-state file)))
	(not (memq state '(nil unregistered ignored)))))))

(defun vc-accurev-state (file)
  "Return the current version control state of FILE."
  (let ((status (vc-accurev--get-status-for-file file)))
    (oref status status)))

(defun vc-accurev-dir-status (dir update-function)
  "Return a list of (FILE STATE EXTRA) entries for DIR."
  (funcall update-function
	   (vc-accurev-dir-status--helper dir 't)
	   nil))

(defun vc-accurev-dir-status--helper(dir-or-files &optional recursive)
  "Produce RESULT: a list of lists of the form (FILE VC-STATE
EXTRA) for the files in DIR.  EXTRA can be used for backend
specific information about FILE.  If a command needs to be run to
compute this list, it should be run asynchronously
using (current-buffer) as the buffer for the command.  When
RESULT is computed, it should be passed back by doing: (funcall
UPDATE-FUNCTION RESULT nil).  If the backend uses a process
filter, hence it produces partial results, they can be passed
back by doing: (funcall UPDATE-FUNCTION RESULT t) and then do
a (funcall UPDATE-FUNCTION RESULT nil) when all the results have
been computed.  To provide more backend specific functionality
for `vc-dir' the following functions might be needed:
`dir-extra-headers', `dir-printer', `extra-dir-menu' and
`dir-status-files'."
  (let ((result nil)
	(status (vc-accurev--get-status dir-or-files recursive)))
    (dolist (x status)
      (push (list (oref x file)
		  (oref x status))
	    result))
    result))

(defun vc-accurev-dir-status-files (dir files default-state update-function)
  "This function is identical to dir-status except that it should
only report status for the specified FILES. Also it needs to
report on all requested files, including up-to-date or ignored
files. If not provided, the default is to consider that the files
are in DEFAULT-STATE."
  (funcall update-function
	   (vc-accurev-dir-status--helper dir)
	   nil))

(defun vc-accurev-working-revision (file)
  "Return the working revision of FILE."
  (let ((status (vc-accurev--get-status-for-file file)))
    (oref status named-revision)))

(defun vc-accurev-revision-granularity () 'file)

(defun vc-accurev-checkout-model (file)
  "Accurev specific version of `vc-checkout-model'."
  'implicit)

;;;
;;; State-changing functions
;;;

(defun vc-accurev-register (files &optional rev comment)
  "Register FILE into the Accurev version-control system.
COMMENT can be used to provide an initial description of FILE."
  (when rev (error "Can't register explicit revision with Accurev"))
  (let (message-file)
    (apply 'vc-accurev-command "add" nil 0 files
	   (if comment
	       (progn
		 (setq message-file (make-temp-file "vc-accurev"))
		 (with-temp-file message-file (insert comment))
		 (list "-c" (format "@%s" message-file)))))
    (vc-exec-after `(when ,message-file (delete-file ,message-file)))))

(defun vc-accurev-init-revision ()
  "The initial revision to use when registering FILE if one is
not specified by the user.  If not provided, the variable
vc-default-init-revision is used instead."
  nil)

(defalias 'vc-accurev-responsible-p 'vc-accurev-root
  "Return non-nil if Accurev considers itself
\"responsible\" for FILE, which can also be a directory.  This
function is used to find out what backend to use for registration
of new files and for things like change log generation.")

(defalias 'vc-accurev-could-register 'vc-accurev-root
  "Return non-nil if FILE could be registered in Accurev.
This is only possible if Accurev is responsible for FILE's directory.")

(defun vc-accurev-checkin (files rev comment)
  "Check FILE(S) into Accurev with log message COMMENT."
  (when rev (error "Cannot commit to a specific revision number"))
  (let (message-file)
    (apply 'vc-accurev-command "keep" nil 0 files
	   (if comment
	       (progn
		 (setq message-file (make-temp-file "vc-accurev"))
		 (with-temp-file message-file (insert comment))
		 (list "-c" (format "@%s" message-file)))))
    (vc-exec-after `(when ,message-file (delete-file ,message-file)))))


(defun vc-accurev-find-revision (file rev buffer)
  "Fetch revision REV of file FILE and put it into BUFFER.
If REV is the empty string, fetch the head of the trunk."
  (with-current-buffer buffer
    (if (and rev (stringp rev) (not (string= rev "")))
	(vc-accurev-command "cat" t 0 file "-v" rev)
      (vc-accurev-command "cat" t 0 file))))

(defun vc-accurev-checkout (file &optional editable rev)
  "Check out revision REV of FILE into the working area.  If EDITABLE
is non-nil, FILE should be writable by the user and if locking is
used for FILE, a lock should also be set.  If REV is non-nil, that
is the revision to check out (default is the working revision).
If REV is t, that means to check out the head of the current branch;
if it is the empty string, check out the head of the trunk.

Currently, this only works for workspace that don't do locking.
In the future we should take into account a workspace that does
require a co or anchor."
  (if rev (error "Operation not supported")
    nil))

(defun vc-accurev-revert (file &optional contents-done)
  "Revert FILE back to the working revision.  If optional
arg CONTENTS-DONE is non-nil, then the contents of FILE have
already been reverted from a version backup, and this function
only needs to update the status of FILE within the backend.
If FILE is in the `added' state it should be returned to the
`unregistered' state.

Currently, this only works for workspaces that don't do locking.
In the future we should take into account a workspace that does
require a co or anchor."
  (unless contents-done
    (with-temp-buffer (vc-accurev-command "purge" t 0 file))))

(defun vc-accurev-update (file editable rev switches)
  (if (and (file-exists-p file) (not rev))
      ;; If no revision was specified, just make the file writable
      ;; if necessary (using `cvs-edit' if requested).
      (and editable (not (eq (vc-accurev-checkout-model file) 'implicit))
	   (if vc-accurev-use-edit
	       (vc-accurev-command nil 0 file "edit")
	     (set-file-modes file (logior (file-modes file) 128))
	     (if (equal file buffer-file-name) (toggle-read-only -1))))
    ;; Check out a particular version (or recreate the file).
    (vc-file-setprop file 'vc-workfile-version nil)
    (apply 'vc-accurev-command nil 0 file
	   (if editable "-w")
	   "update"
	   ;; default for verbose checkout: clear the sticky tag so
	   ;; that the actual update will get the head of the trunk
	   (if (or (not rev) (string= rev ""))
	       "-A"
	     (concat "-r" rev))
	   switches)))

(defun vc-accurev-merge (file first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (vc-accurev-command nil 0 file
		   "update" "-kk"
		   (concat "-j" first-version)
		   (concat "-j" second-version))
  (vc-file-setprop file 'vc-state 'edited)
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    (if (re-search-forward "conflicts during merge" nil t)
        1				; signal error
      0)))				; signal success

(defun vc-accurev-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-workfile-version nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-accurev-command nil 0 file "update")
  ;; Analyze the merge result reported by Meta-CVS, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new workfile version
    (if (re-search-forward
	 "^Merging differences between [0-9.]* and \\([0-9.]*\\) into" nil t)
	(vc-file-setprop file 'vc-workfile-version (match-string 1))
      (vc-file-setprop file 'vc-workfile-version nil))
    ;; get file status
    (prog1
        (if (eq (buffer-size) 0)
            0 ;; there were no news; indicate success
          (if (re-search-forward
               (concat "^\\([CMUP] \\)?"
                       ".*"
                       "\\( already contains the differences between \\)?")
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((or (match-string 2)
                    (string= (match-string 1) "U ")
                    (string= (match-string 1) "P "))
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 1) "M ")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'edited)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze accurev update result")))
      (message "Merging changes into %s...done" file))))

;;;
;;; History functions
;;;

(defun vc-accurev-print-log (files buffer &optional shortlog start-revision limit)
  "Insert the revision log for FILES into BUFFER, or the *vc* buffer
if BUFFER is nil. If SHORTLOG is true insert a short version of the log."
  ;; I'm not sure how useful shortlog is.  Directories are real
  ;; elements and have histories; accurev doesn't recurse into them
  ;; for the histories of other children elements like other vcs.
  (vc-setup-buffer buffer)
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
      (vc-accurev-command "hist" buffer 'async files
			  (if shortlog "-ft")))))

(define-derived-mode vc-accurev-log-view-mode log-view-mode "Accurev-Log-View"
  (require 'add-log) ;; we need the faces add-log
  ;; Don't have file markers, so use impossible regexp.
  (set (make-local-variable 'log-view-file-re) "^[ \t]*element:[ \t]+\\([^\n]+\\)")
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-message-re) "^[ \t]*transaction[ \t]+\\([0-9]+\\);")
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append
	`((,log-view-file-re
	   (1 'log-view-file))
	  (,log-view-message-re
	   (1 'change-log-acknowledgement)
	   ("[ \t]+\\([^;]+\\);[ \t]+\\([0-9/]+ [0-9:]+\\)[ \t]+;[ \t]+user:[ \t]+\\([^;\n]+\\)" nil nil
	    (2 'change-log-date)
	    (3 'change-log-name))
	   ))
        '(("^[ \t]*eid:[ \t]\\([0-9]+\\)"
	   (1 'log-view-file))
	  ("^[ \t]+#\\([^\n]*\\)"
	   (1 'log-view-message))
	  ("^[ \t]+version \\([0-9]+/[0-9]+\\) (\\([0-9]+/[0-9]+\\))"
	   (1 'change-log-acknowledgement)
	   (2 'change-log-list))
	  ("^[ \t]+ancestor:[ \t]+(\\([0-9]+/[0-9]+\\))"
           (1 'change-log-function)
	   ("[ \t]+merged against:[ \t]+(\\([0-9]+/[0-9]+\\))" nil nil
	    (1 'change-log-conditionals)))))))

(defun vc-accurev-show-log-entry (revision)
  "Find entry for patch name REVISION in accurev change log buffer."
  (goto-char (point-min))
  (when revision
    (let (case-fold-search)
      (if (re-search-forward
           (concat "^[ \t]+version " (regexp-quote revision)) nil t)
          (beginning-of-line 0)
        (goto-char (point-min))))))

;;;
;;; Diff
;;;

(defun vc-accurev-diff (files &optional rev1 rev2 buffer)
  "Insert the diff for FILE into BUFFER, or the *vc-diff* buffer if
   BUFFER is nil.  If REV1 and REV2 are non-nil, report differences
   from REV1 to REV2.  If REV1 is nil, use the working revision (as
   found in the repository) as the older revision; if REV2 is nil,
   use the current working-copy contents as the newer revision.  This
   function should pass the value of (vc-switches BACKEND 'diff) to
   the backend command.  It should return a status of either 0 (no
   differences found), or 1 (either non-empty diff or the diff is
   run asynchronously)."
  (apply 'vc-accurev-command "diff" (or buffer "*vc-diff*") async files 
	 (append
	  (if (not (or rev1 rev2)) (list "-b")) ;; diff to basis version
	  (if rev1 (list "-v" rev1))
	  (if rev2 (list "-V" rev2)))))

(defun vc-accurev-revision-completion-table (files)
  "Return a completion table for existing revisions of FILES.
   The default is to not use any completion table."
  (lexical-let ((files files)
		table)
    (setq table (lazy-completion-table
		 table (lambda () (vc-accurev--revision-table files))))
    table))

(defun vc-accurev--revision-table (files)
  "Return a completion table for existing revisions of FILES.
   This currently returns virtual versions."
  (with-temp-buffer
    (vc-accurev-command "hist" t nil files "-fx")
    (goto-char (point-min))
    (let ((ids ()))
      (while (re-search-forward "\\(?:virtualNamedVersion\\|virtual\\)=\"\\([^\"]+\\)\"" nil t)
	(push (match-string 1) ids))
      ids)))

;;;
;;; Annotate
;;;

(defun vc-accurev-annotate-command (file buffer &optional version)
  "Execute \"accurev annotate\" on FILE, inserting the contents in BUFFER.
Optional arg VERSION is a version to annotate from."
  (vc-accurev-command "annotate" buffer 0 file "-fuvd" (if version
							  (concat "-v" version))))

(defconst vc-accurev-annotate-time-regex "^\\S-+\\s-+\\S-+\\s-+\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)\\s-+\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)")

(defun vc-accurev-annotate-time ()
  "Return the time of the next annotation (as fraction of days)
systime, or nil if there is none."
  (let* ((bol (point))
	 (cache (get-text-property bol 'vc-accurev-annotate-time))
	 buffer-read-only)
    (cond (cache)
	  ((looking-at vc-accurev-annotate-time-regex)
	   (let ((year (string-to-number (match-string 1)))
		 (month (string-to-number (match-string 2)))
		 (day (string-to-number (match-string 3)))
		 (hour (string-to-number (match-string 4)))
		 (minute (string-to-number (match-string 5)))
		 (second (string-to-number (match-string 6))))
	     (put-text-property
	      bol (1+ bol) 'vc-accurev-annotate-time
	      (setq cache (cons
			   (match-end 0)
			   (vc-annotate-convert-time
			    (encode-time second minute hour day month year))))))))
    (when cache
      (goto-char (car cache))
      (cdr cache))))

(defun vc-accurev-annotate-extract-revision-at-line ()
  "Return the revision corresponding to the current line, or nil
if there is no revision corresponding to the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^\\S-+\\s-+\\([0-9]+/[0-9]+\\)" (line-end-position) t)
	(match-string-no-properties 1)
      nil)))
		       
;;;
;;; Miscellaneous
;;;
(defun vc-accurev-root (file)
  "Return the root of the VC controlled hierarchy for file."
  (let (root)
    (mapc (lambda (x)
	    (if (string-match (regexp-quote (vc-accurev-workspace->location x)) file)
		(setq root (vc-accurev-workspace->location x))))
	  (vc-accurev--get-workspaces))
    root))

(defun vc-accurev-repository-hostname (dirname)
  "Return the hostname that the backend will have to contact in
order to operate on a file in DIRNAME.  If the return value is
nil, it means that the repository is local."
  (let ((info (vc-accurev--get-info file)))
    (concat (vc-accurev-info->host info)
	    ":"
	    (vc-accurev-info->port info))))

(defun vc-accurev-delete-file (file)
  "Delete FILE and mark it as deleted in the repository."
  (vc-accurev-command "defunct" nil 0 file))

(defun vc-accurev-rename-file (old new)
  "Rename file OLD to NEW, both in the working area and in the
repository."
  (vc-accurev-command "move" nil 0 new old))

(defun vc-accurev-previous-revision (file rev)
  "Return the revision number that precedes REV for FILE, or nil if no such
revision exists."
  (with-temp-buffer
    (vc-accurev-command "anc" t 0 file "-fx" (when rev (concat "-v" rev)))
    (let ((anc (vc-accurev--parse-xml 'vc-accurev-ancestor nil nil
                                      (lambda (results) (car results))
                                      :file file)))
      (if (not (null anc))
          (format "%s/%s" (oref anc stream) (oref anc version))
        anc))))
;;;
;;; Snapshot system
;;;

(defun vc-accurev-create-snapshot (dir name branchp)
  "Create a snapshot from the current DIR's basis stream with the given NAME.
If BRANCHP is non-nil, the current workspace is moved to that new
stream."
  (let ((basis (vc-accurev-basis-stream dir))
	(default-directory dir))
    (vc-accurev-command "mkstream" t 0 nil "-s" name "-b" basis))
  (when branchp (vc-accurev-retrieve-snapshot dir name nil)))
    
(defun vc-accurev-retrieve-snapshot (dir name update)
  "Retrieve a snapshot at and below DIR.
NAME is the name of the snapshot; if it is empty, do a `cvs update'.
If UPDATE is non-nil, then update (resynch) any affected buffers."
  (with-current-buffer (get-buffer-create "*vc*")
    (let ((default-directory dir)
	  (workspace (vc-accurev-workspace-name dir)))
      (erase-buffer)
      (if (and name (not (string= name "")))
	  (vc-accurev-command "chws" t 0 nil "-w" workspace "-b" name))
      (vc-accurev-command "update" t 0 nil)
      (when update
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at (concat "\\("
				  "Removing " "\\|"
				  "Content ([^)]*) of " "\\|"
				  "Creating dir " "\\)"
				  "\"\\([^\"]*\""))
	      (let* ((file (expand-file-name (match-string 2) dir))
		     (state (match-string 1))
		     (buffer (find-buffer-visiting file)))
		(when buffer
		  (cond
		   ((string-match "Content\\|Creating" state)
		    (vc-file-setprop file 'vc-state 'up-to-date)
		    (vc-file-setprop file 'vc-workfile-version nil)
		    (vc-file-setprop file 'vc-checkout-time
				     (nth 5 (file-attributes file)))))
		  (vc-file-setprop file 'vc-accurev-backing-stream name)
		  (vc-file-setprop file 'vc-accurev-workspace workspace)
		  (vc-resynch-buffer file t t))))
	  (forward-line 1))))))

;;;
;;; Internal functions
;;;

(defun vc-accurev-command (accurev-command buffer okstatus file-or-list &rest args)
  "A wrapper around `vc-do-command' for use in vc-accurev.el."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-accurev-program
	  file-or-list accurev-command args))

(defun vc-accurev--parse-xml (obj &optional buffer error-function function &rest init-args)
  "Simple case flow for parsing XML responses with objects"
  (cond ((null buffer) (setq buffer (current-buffer)))
        ((null error-function) (setq error-function 'identity))
        ((null function) (setq function 'identity)))
  (condition-case var
      (with-current-buffer buffer
        (let ((results '())
              (str (xml-parse-region (point-min) (point-max)))
              (node-name (oref (make-instance obj) node-name)))
          (dolist (element (xml-get-children (xml-node-name str) node-name))
            (let ((instance (apply 'make-instance (append `(,obj) init-args))))
              (vc-accurev--parse instance element)
              (add-to-list 'results instance 't)))
          (funcall function results)))
    (error (format "ERROR: %s" var))))
;     (funcall error-function))))

(defun vc-accurev--get-status-for-file (file &optional flags function)
  "Retrieve status information about a single FILE."
  (let ((status (vc-accurev--get-status file nil flags function)))
    (if (= (length status) 1)
        (car x)
      x)))

(defun vc-accurev--get-status (files &optional recursive flags function)
  "Retrieve all status information about FILES.  This drives other information services."
  (with-temp-buffer
    (vc-accurev-command "stat" t 0 files "-fxr" (when recursive "-R") flags)
    (vc-accurev--parse-xml 'vc-accurev-status nil nil function)))

(defun vc-accurev--get-workspaces (&optional function)
  (condition-case ()
      (let ((results '())
	    str)
	(with-temp-buffer
	  (vc-accurev-command "show" t 0 nil "wspaces" "-fx")
	  (setq str (xml-parse-region (point-min) (point-max))))
	(dolist (element (xml-get-children (xml-node-name str) 'Element))
	  (let ((wspace (vc-accurev-workspace-create)))
	    (add-to-list 'results wspace 't)
	    (setf (vc-accurev-workspace->name wspace) (xml-get-attribute-or-nil element 'Name))
	    (setf (vc-accurev-workspace->location wspace) (xml-get-attribute-or-nil element 'Storage))
	    (setf (vc-accurev-workspace->host wspace) (xml-get-attribute-or-nil element 'Host))
	    (setf (vc-accurev-workspace->stream wspace) (xml-get-attribute-or-nil element 'Stream))
	    (setf (vc-accurev-workspace->depot wspace) (xml-get-attribute-or-nil element 'depot))
	    (setf (vc-accurev-workspace->user wspace) (xml-get-attribute-or-nil element 'user_name))))
	(funcall (if (null function) 'identity function) results))
    (error)))

(defun vc-accurev--parse-nested-statuses (stati)
  "Convert a list of accurev statuses into vc states"
  (let ((translation '(("backed" up-to-date . 0)
		       ("modified" edited . 1) 
		       ("stale" needs-update . 2)
		       ("overlap" needs-merge . 3)
		       ("underlap" needs-merge . 3)
		       ("member" added . 4)
		       ("kept" added . 4)
		       ("defunct" removed . 5)
		       ("missing" missing . 6)
		       ("excluded" missing . 6)
		       ("external" unregistered . 10)
		       ("no such elem" unregistered . 10)))
	(str stati)
	(result nil)
	(rest nil))
    (while (string-match "(\\([^)]*\\))\\(.*\\)" str)
      (let ((match (cdr (assoc (match-string 1 str) translation))))
	(setq str (match-string 2 str))
	(cond ((null match))
		; do nothing
	      ((null result)
		(setq result match))
	      ((< (cdr result) (cdr match))
		(push result rest)
		(setq result match))
	      (t
		(push match rest)))))
    (list (car result) (mapcar (lambda (x) (car x)) rest))))

(defun vc-accurev--parse-info (info)
  "Create a info structure from accurev's output"
  (goto-char (point-min))
  (while (not (eobp))
    (cond ((looking-at "Shell:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->shell info) (match-string 1)))
	  ((looking-at "Principal:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->principal info) (match-string 1)))
	  ((looking-at "Host:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->host info) (match-string 1)))
	  ((looking-at "Domain:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->domain info) (match-string 1)))
	  ((looking-at "client_ver:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->client-version info) (match-string 1)))
	  ((looking-at "Server name:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->server-name info) (match-string 1)))
	  ((looking-at "Port:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->port info) (match-string 1)))
	  ((looking-at "ACCUREV_BIN:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->bin info) (match-string 1)))
	  ((looking-at "server_ver:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->server-version info) (match-string 1)))
	  ((looking-at "Client time:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->client-time info) (match-string 1)))
	  ((looking-at "Server time:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->server-time info) (match-string 1)))
	  ((looking-at "Depot:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->depot info) (match-string 1)))
	  ((looking-at "Workspace/ref:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->workspace info) (match-string 1)))
	  ((looking-at "Basis:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->basis info) (match-string 1)))
	  ((looking-at "Top:\\s-+\\(.+\\)")
	   (setf (vc-accurev-info->top info) (match-string 1))))
    (forward-line 1)))

(defun vc-accurev--get-info (file)
  "Create a info structure from accurev's output"
  (let ((default-directory file)
	(info (vc-accurev-create-info)))
    (with-temp-buffer
      (vc-accurev-command "info" 't 0 file "-v")
      (vc-accurev--parse-info info)
      info)))

;;;
;;; Intermediate Structures
;;;
(defstruct (vc-accurev-workspace
	    (:copier nil)
	    (:type list)
	    (:constructor vc-accurev-workspace-create (&optional name location host))
	    (:conc-name vc-accurev-workspace->))
  name location host stream depot user)

(defstruct (vc-accurev-info
	    (:copier nil)
	    (:type list)
	    (:constructor vc-accurev-create-info (&optional top depot workspace basis
							    server-name server-version))
	    (:conc-name vc-accurev-info->))
  top ;; root directory for the project workspace
  depot workspace basis server-name server-version principal
  domain port client-version server-time client-time
  host bin shell)

(defclass vc-accurev-object ()
  ((node-name :initform 'element :type symbol :allocation :class
              :documentation "XML node name"))
  :abstract t)

(defclass vc-accurev-ancestor (vc-accurev-object)
  ((file :init-arg :file)
   (location :init-arg :location)
   (stream :init-arg :stream)
   (version :init-arg :version))
  :documentation "")

(defclass vc-accurev-status ()
  ((file :init-arg :file)
   (real-revision :init-arg :real-revision)
   (named-revision :init-arg :named-revision)
   (virtual-revison :init-arg :virtual-revison)
   (element-id :init-arg :element-id)
   (element-type :init-arg :element-type)
   (status :init-arg :status)
   (extra-status :init-arg :extra-status)
   (directory-p :init-arg :directory-p)
   (hierarchy-type :init-arg :hierarchy-type)
   (size :init-arg :size)
   (modified-time :init-arg :modified-time))
  :documentation "")

(defmethod vc-accurev--parse ((ancestor vc-accurev-object) element)
  "Base class parser; currently does nothing")

(defmethod vc-accurev--parse ((ancestor vc-accurev-ancestor) element)
  "Set slot values from the supplied XML element"
  (oset ancestor location (xml-get-attribute-or-nil element 'location))
  (oset ancestor stream (xml-get-attribute-or-nil element 'stream))
  (oset ancestor version (xml-get-attribute-or-nil element 'version)))

(defmethod vc-accurev--parse ((status vc-accurev-status) element)
  "Set slot values from the supplied output element"
  (let ((stati (vc-accurev--parse-nested-statuses (xml-get-attribute-or-nil element 'status))))
    (oset status file (xml-get-attribute-or-nil element 'location)
    (oset status real-revision (xml-get-attribute-or-nil element 'Real))
    (oset status named-revision (xml-get-attribute-or-nil element 'namedVersion))
    (oset status virtual-revison (xml-get-attribute-or-nil element 'Virtual))
    (oset status element-id (xml-get-attribute-or-nil element 'id))
    (oset status element-type (xml-get-attribute-or-nil element 'elemType))
    (oset status status (car stati))
    (oset status extra-status (cadr stati))
    (oset status size (xml-get-attribute-or-nil element 'size))
    (oset status directory-p (xml-get-attribute-or-nil element 'dir))
    (oset status hierarchy-type (xml-get-attribute-or-nil element 'hierType))
    (oset status modified-time (xml-get-attribute-or-nil element 'modTime)))))

(provide 'vc-accurev)

;;; vc-accurev.el ends here
