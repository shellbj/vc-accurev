# vc-accurev

An Accurev VCS (accurev) backend for Emacs standard Version Control
module; allows adding, commiting, reverting and updating from within
the Emacs session.

## How to use

Add to the load path and require the module.

```lisp
(add-to-list 'load-path "~/.emacs.d/vc-accurev")
(require 'vc-accurev)
```

## Note

This was developed against version 4.8.0 of accurev and the nightly
versions of emacs (24.3).  As of this writing (2012) I no longer have
access to accurev to do any additional work.

