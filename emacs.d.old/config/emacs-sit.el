;;; emacs-sit.el --- Make emacs sit where you tell it

(defvar emacs-sit-root-dir (expand-file-name (file-name-as-directory command-line-default-directory)))

(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory emacs-sit-root-dir)))

(add-hook 'after-save-hook
          (lambda ()
            (if (not (eq default-directory emacs-sit-root-dir))
                (setq default-directory emacs-sit-root-dir))))

(defun emacs-sit-cd (dir)
  "Make DIR become the all file buffer's default directory.
If your environment includes a `CDPATH' variable, try each one of
that list of directories (separated by occurrences of
`path-separator') when resolving a relative directory name.
The path separator is colon in GNU and GNU-like systems."
  (interactive
   (list
    ;; FIXME: There's a subtle bug in the completion below.  Seems linked
    ;; to a fundamental difficulty of implementing `predicate' correctly.
    ;; The manifestation is that TAB may list non-directories in the case where
    ;; those files also correspond to valid directories (if your cd-path is (A/
    ;; B/) and you have A/a a file and B/a a directory, then both `a' and `a/'
    ;; will be listed as valid completions).
    ;; This is because `a' (listed because of A/a) is indeed a valid choice
    ;; (which will lead to the use of B/a).
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuffer-completion-table
                (apply-partially #'locate-file-completion-table
                                 cd-path nil))
          (setq minibuffer-completion-predicate
                (lambda (dir)
                  (locate-file dir cd-path nil
                               (lambda (f) (and (file-directory-p f) 'dir-ok))))))
      (unless cd-path
        (setq cd-path (or (parse-colon-path (getenv "CDPATH"))
                          (list "./"))))
      (read-directory-name "Change default directory: "
                           default-directory default-directory
                           t))))
  (unless cd-path
    (setq cd-path (or (parse-colon-path (getenv "CDPATH"))
                      (list "./"))))
  (emacs-sit-cd-absolute
   (or (locate-file dir cd-path nil
                    (lambda (f) (and (file-directory-p f) 'dir-ok)))
       (error "No such directory found via CDPATH environment variable"))))

(defun emacs-sit-cd-absolute (dir)
  "Change emacs-sit root directory to given absolute file name DIR,
and then change the default directory for all buffers."
  ;; Put the name into directory syntax now,
  ;; because otherwise expand-file-name may give some bad results.
  (setq dir (file-name-as-directory dir))
  ;; We used to additionally call abbreviate-file-name here, for an
  ;; unknown reason.  Problem is that most buffers are setup
  ;; without going through cd-absolute and don't call
  ;; abbreviate-file-name on their default-directory, so the few that
  ;; do end up using a superficially different directory.
  (setq dir (expand-file-name dir))
  (setq emacs-sit-root-dir dir)
  (if (not (file-directory-p dir))
      (if (file-exists-p dir)
          (error "%s is not a directory" dir)
        (error "%s: no such directory" dir))
    (unless (file-accessible-directory-p dir)
      (error "Cannot cd to %s:  Permission denied" dir))
    (setq default-directory dir)
    (setq list-buffers-directory dir)
    (dolist (b (buffer-list))
        (with-current-buffer b
            (if (buffer-file-name b)
                (setq default-directory emacs-sit-root-dir)
            )))
    ))

(provide 'emacs-sit)
