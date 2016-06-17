(require 'packages)

(require 'emacs-sit)

(setq vc-handled-backends nil)

(add-to-list 'load-path
             (expand-file-name "config/development" user-emacs-directory))

(ensure-package-installed 'flycheck)
(require 'flycheck)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq flycheck-display-errors-delay 0.1)

(global-flycheck-mode)

(require 'evil)
(evil-ex-define-cmd "refreshs[yntax]" 'font-lock-fontify-buffer)
(evil-ex-define-cmd "cd" 'emacs-sit-cd)

(show-paren-mode 1)
(if (boundp 'show-paren-delay)
    (setq show-paren-delay 0))

;; Language packages
(ensure-package-installed 'scala-mode
                          'ensime)

(require 'cmake-mode)
(require 'lisp-interaction)

(provide 'development)
