(require 'packages)

(ensure-package-installed 'flycheck)
(require 'flycheck)
(with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(global-flycheck-mode)

(provide 'development)
