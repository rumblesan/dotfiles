;;; emacs-sit.el --- Make emacs sit where you tell it

(defvar emacs-sit-root-dir (expand-file-name (file-name-as-directory command-line-default-directory)))

(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory emacs-sit-root-dir)))

;; Redefine cd-absolute so that more changing
;; directory functionality will work how we want.. I think
(eval-after-load "files"
  '(defun cd-absolute (dir)
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
       (mapc (lambda (b)
               (with-current-buffer b
                 (if (buffer-file-name b)
                     (setq default-directory emacs-sit-root-dir)
                   )
                 )
               )
             (buffer-list)
             )
       )))

(provide 'emacs-sit)
