(require 'packages)

(require 'esc-to-escape-setup)
(require 'tmux-navigate)

(use-package helm
  :init
  (use-package shackle
    :config
    ;; Keep helm window in check
    (setq shackle-rules '(
                          ("\\`\\*helm.*?\\*\\'" :regexp t :align 'below :size 0.4)
                          ("\\`\\*ENSIME.*?\\*\\'" :regexp t :align 'below :size 0.4)
                          ))
    (setq helm-split-window-preferred-function 'ignore)
    (shackle-mode)
    )
  :config
  (require 'helm-unite)
  (require 'helm-config)
  (setq helm-display-header-line nil)
  (setq helm-mode-line-string "")
  (helm-mode 1)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'unite-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  )

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

;; Setup evil
(use-package evil
  :init
  ;; Load evil-leader before evil so it works in all buffers
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "f" 'unite-find-files
      "b" 'list-buffers
      "y" 'pbcopy
      "p" 'pbpaste
      )
    )

  :config
  (define-key evil-motion-state-map ";" 'evil-ex)

  ;; General niceties
  (setq evil-vsplit-window-right 'left)
  (setq evil-split-window-below 'above)

  ;; Handle esc-to-escape mapping outselves so we get it in all modes
  (setq evil-esc-mode -1)

  ;; Use evil everywhere
  (evil-mode t)
  ;; Esc quits properly
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)
  )

(provide 'navigation)
