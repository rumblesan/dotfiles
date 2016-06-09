(require 'packages)

(ensure-package-installed 'flycheck)
(require 'flycheck)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq flycheck-display-errors-delay 0.1)

(global-flycheck-mode)

(show-paren-mode 1)
(if (boundp 'show-paren-delay)
    (setq show-paren-delay 0))

(provide 'development)
