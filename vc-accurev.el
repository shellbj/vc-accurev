;;; vc-accurev.el --- VC backend for the Accurev version-control system

;; Copyright (C) 2008 Bryan Shell

;; Author:      Bryan Shell
;; Maintainer:  Bryan Shell

;;; Commentary:

;;; Bugs:

;;; Code:

(eval-when-compile (require 'vc))

;;;
;;; Customization options
;;;

(defvar vc-accurev-command
  (let ((candidates '("accurev")))
    (while (and candidates (not (executable-find (car candidates))))
      (setq candidates (cdr candidates)))
    (or (car candidates) "accurev")))

;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-accurev-registered (file)
;;;###autoload   (if (executable-find "accurev")
;;;###autoload       (progn
;;;###autoload 	       (load "vc-accurev")
;;;###autoload 	       (vc-accurev-registered file))))

 (defun vc-accurev-registered (file)
   (if (vc-accurev-root file)
       (progn
         (load "vc-accurev")
         (vc-accurev-registered file))))

(defun vc-accurev-root (file)
  "Return the root directory of a accurev project."
  (or (vc-file-getprop file 'accurev-root)
      (vc-file-setprop file 'accurev-root (with-temp-buffer
					    (let ((default-directory file))
					      (vc-accurev-command t 0 file "info" "-v")
					      (goto-char (point-min))
					      (when (re-search-forward "^Top:[[:space:]]+\\(.+\\)$" nil t)
						(match-string 1)))))))

(defun vc-accurev-repository-hostname (file)
  "Return the accurev server hostname."
  (or (vc-file-getprop file 'accurev-repository-hostname)
      (vc-file-setprop file 'accurev-repository-hostname
		       (with-temp-buffer
			 (setq default-directory dirname)
			 (vc-accurev-command t 0 file "info")
			 (goto-char (point-min))
			 (when (re-search-forward "Server name:[[:space:]]+\\(.+\\)" nil t)
			   (match-string 1))))))

(defun vc-accurev-workspace-name (file)
  "Return the accurev server hostname."
  (or (vc-file-getprop file 'accurev-workspace-name)
      (vc-file-setprop file 'accurev-workspace-name
		       (with-temp-buffer
			 (setq default-directory dirname)
			 (vc-accurev-command t 0 file "info")
			 (goto-char (point-min))
			 (when (re-search-forward "Workspace/ref:[[:space:]]+\\(.+\\)" nil t)
			   (match-string 1))))))

(defun vc-accurev-basis-stream (file)
  "Return the accurev server hostname."
  (or (vc-file-getprop file 'accurev-basis-stream)
      (vc-file-setprop file 'accurev-basis-stream
		       (with-temp-buffer
			 (setq default-directory dirname)
			 (vc-accurev-command t 0 file "info")
			 (goto-char (point-min))
			 (when (re-search-forward "Basis:[[:space:]]+\\(.+\\)" nil t)
			   (match-string 1))))))


(defun vc-accurev-registered (file)
  (vc-accurev-state file)
  (vc-accurev-root file))

(defun vc-accurev-state (file &optional localp)
  ;; This would assume the Meta-CVS sandbox is synchronized.
  ;; (vc-accurev-cvs state file))
  "Meta-CVS-specific version of `vc-state'."
  (setq localp (or localp (vc-stay-local-p file)))
  (let ((default-directory dir))
    (with-temp-buffer
      (vc-accurev-command t 0 file "stat" "-fr")
      (vc-accurev-parse-status file)))

(defun vc-accurev-dir-state (dir &optional localp)
  ;; This would assume the Meta-CVS sandbox is synchronized.
  ;; (vc-accurev-cvs state file))
  "Meta-CVS-specific version of `vc-state'."
  (setq localp (or localp (vc-stay-local-p file)))
  (let ((default-directory dir))
    (with-temp-buffer
      (vc-accurev-command t 0 file "stat" "-fr" (if localp "-m" "-a"))
      (vc-accurev-parse-status))))

(defun vc-accurev-workfile-version (file)
  (vc-accurev-registered file)
  (vc-file-getprop file 'vc-workfile-version))

(defun vc-accurev-checkout-model (file)
  "Accurev specific version of `vc-checkout-model'."
  'implicit)

;;;
;;; State-changing functions
;;;

(defun vc-accurev-register (file &optional rev comment)
  "Register FILE into the Accurev version-control system.
COMMENT can be used to provide an initial description of FILE.

`vc-register-switches' and `vc-accurev-register-switches' are passed to
the Accurev command (in that order)."
  (apply 'vc-accurev-command nil 0 file "add"
	 (if comment (concat "-c" comment))
	 (vc-switches 'ACCUREV 'register)))

(defalias 'vc-accurev-responsible-p 'vc-accurev-root
  "Return non-nil if CVS thinks it is responsible for FILE.")

(defalias 'vc-cvs-could-register 'vc-cvs-responsible-p
  "Return non-nil if FILE could be registered in Meta-CVS.
This is only possible if Meta-CVS is responsible for FILE's directory.")

(defun vc-accurev-checkin (file rev comment)
  "Meta-CVS-specific version of `vc-backend-checkin'."
  (unless (or (not rev) (vc-accurev-valid-version-number-p rev))
    (if (not (vc-accurev-valid-symbolic-tag-name-p rev))
	(error "%s is not a valid symbolic tag name" rev)
      ;; If the input revision is a valid symbolic tag name, we create it
      ;; as a branch, commit and switch to it.
      ;; This file-specific form of branching is deprecated.
      ;; We can't use `accurev branch' and `accurev switch' because they cannot
      ;; be applied just to this one file.
      (apply 'vc-accurev-command nil 0 file "tag" "-b" (list rev))
      (apply 'vc-accurev-command nil 0 file "update" "-r" (list rev))
      (vc-file-setprop file 'vc-accurev-sticky-tag rev)
      (setq rev nil)))
  ;; This commit might cvs-commit several files (e.g. MAP and TYPES)
  ;; so using numbered revs here is dangerous and somewhat meaningless.
  (when rev (error "Cannot commit to a specific revision number"))
  (let ((status (apply 'vc-accurev-command nil 1 file
		       "ci" "-m" comment
		       (vc-switches 'ACCUREV 'checkin))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (when (not (zerop status))
      ;; Check checkin problem.
      (cond
       ((re-search-forward "Up-to-date check failed" nil t)
        (vc-file-setprop file 'vc-state 'needs-merge)
        (error (substitute-command-keys
                (concat "Up-to-date check failed: "
                        "type \\[vc-next-action] to merge in changes"))))
       (t
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)
        (error "Check-in failed"))))
    ;; Update file properties
    (vc-file-setprop
     file 'vc-workfile-version
     (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
    ;; Forget the checkout model of the file, because we might have
    ;; guessed wrong when we found the file.  After commit, we can
    ;; tell it from the permissions of the file (see
    ;; vc-accurev-checkout-model).
    (vc-file-setprop file 'vc-checkout-model nil)

    ;; if this was an explicit check-in (does not include creation of
    ;; a branch), remove the sticky tag.
    (if (and rev (not (vc-accurev-valid-symbolic-tag-name-p rev)))
	(vc-accurev-command nil 0 file "update" "-A"))))

(defun vc-accurev-find-version (file rev buffer)
  (apply 'vc-accurev-command
	 buffer 0 file
	 "-Q"				; suppress diagnostic output
	 "update"
	 (and rev (not (string= rev ""))
	      (concat "-r" rev))
	 "-p"
	 (vc-switches 'ACCUREV 'checkout)))

(defun vc-accurev-checkout (file &optional editable rev)
  (message "Checking out %s..." file)
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (vc-call update file editable rev (vc-switches 'ACCUREV 'checkout)))
  (vc-mode-line file)
  (message "Checking out %s...done" file))

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

(defun vc-svn-delete-file (file)
  (vc-svn-command nil 0 file "defunct"))

(defun vc-accurev-rename-file (old new)
  (vc-accurev-command nil 0 new "move" (file-relative-name old)))

(defun vc-accurev-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (vc-default-revert 'ACCUREV file contents-done)
  (unless (eq (vc-checkout-model file) 'implicit)
    (if vc-accurev-use-edit
        (vc-accurev-command nil 0 file "unedit")
      ;; Make the file read-only by switching off all w-bits
      (set-file-modes file (logand (file-modes file) 3950)))))

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

(defun vc-accurev-print-log (file &optional buffer)
  "Insert the revision log of FILE into BUFFER, or the *vc*
buffer if BUFFER is nil."
  (unless buffer (setq buffer "*vc*"))
  (let ((default-directory (vc-accurev-root file)))
    (vc-accurev-command
     buffer
     (if (and (vc-stay-local-p file) (fboundp 'start-process)) 'async 0)
     file "hist" (vc-switches 'ACCUREV 'history))))

;;;
;;; Diff
;;;

(defun vc-accurev-diff (file &optional oldvers newvers buffer)
  "Get a difference report using Accurev between two versions of FILE."
  (unless buffer (setq buffer "*vc-diff*"))
  (if (string= (vc-workfile-version file) "0")
      ;; This file is added but not yet committed; there is no master file.
      (if (or oldvers newvers)
	  (error "No revisions of %s exist" file)
	;; We regard this as "changed".
	;; Diff it against /dev/null.
	;; Note: this is NOT a "accurev diff".
	(apply 'vc-do-command buffer 1 "diff" file
	       (append (vc-switches nil 'diff) '("/dev/null")))
	;; Even if it's empty, it's locally modified.
	1)
    (let* ((async (and (not vc-disable-async-diff)
                       (vc-stay-local-p file)
                       (fboundp 'start-process)))
	   ;; Run the command from the root dir so that `accurev filt' returns
	   ;; valid relative names.
	   (default-directory (vc-accurev-root file))
	   (status
	    (apply 'vc-accurev-command buffer
		   (if async 'async 1)
		   file "diff"
		   (and oldvers (concat "-v" oldvers))
		   (and newvers (concat "-V" newvers))
		   (vc-switches 'ACCUREV 'diff))))
      (if async 1 status))))	       ; async diff, pessimistic assumption.

(defun vc-accurev-diff-tree (dir &optional rev1 rev2)
  "Diff all files at and below DIR."
  (with-current-buffer "*vc-diff*"
    ;; Run the command from the root dir so that `accurev filt' returns
    ;; valid relative names.
    (setq default-directory (vc-accurev-root dir))
    ;; cvs diff: use a single call for the entire tree
    (let ((coding-system-for-read (or coding-system-for-read 'undecided)))
      (apply 'vc-accurev-command "*vc-diff*" 1 dir "diff" "-a"
	     (and rev1 (concat "-v" rev1))
	     (and rev2 (concat "-V" rev2))
	     (vc-switches 'ACCUREV 'diff)))))

;;;
;;; Annotate
;;;

(defun vc-accurev-annotate-command (file buffer &optional version)
  "Execute \"accurev annotate\" on FILE, inserting the contents in BUFFER.
Optional arg VERSION is a version to annotate from."
  (vc-accurev-command buffer 0 file "annotate" "-fuvd" (if version
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
;;; Snapshot system
;;;

(defun vc-accurev-create-snapshot (dir name branchp)
  "Create a snapshot from the current DIR's basis stream with the given NAME.
If BRANCHP is non-nil, the current workspace is moved to that new
stream."
  (let ((basis (vc-accurev-basis-stream dir))
	(default-directory dir))
    (vc-accurev-command t 0 nil "mkstream" "-s" name "-b" basis))
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
	  (vc-accurev-command t 0 nil "chws" "-w" workspace "-b" name))
      (vc-accurev-command t 0 nil "update")
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

(defun vc-accurev-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-accurev.el."
  (apply 'vc-do-command buffer okstatus "accurev" file flags))

(provide 'vc-accurev)

;;; vc-accurev.el ends here
