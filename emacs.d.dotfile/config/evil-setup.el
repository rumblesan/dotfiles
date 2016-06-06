(ensure-package-installed 'evil 'evil-leader)
(require 'evil)
(require 'evil-leader)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'helm-buffers-list
  )

(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)

(define-key evil-motion-state-map ";" 'evil-ex)

(setq evil-shift-width 4)

(setq evil-vsplit-window-right 'left)
(setq evil-split-window-below 'above)

(setq evil-esc-mode -1)
;; Handle esc-to-escape mapping outselves so we get it in all modes
(require 'esc-to-escape-setup)

;; esc quits
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

(global-evil-leader-mode)
(evil-mode t)

(provide 'evil-setup)
