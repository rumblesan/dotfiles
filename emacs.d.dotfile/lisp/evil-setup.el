(ensure-package-installed 'evil 'evil-leader)
(require 'evil)
(require 'evil-leader)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "f" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)

(define-key evil-motion-state-map ";" 'evil-ex)

(setq evil-shift-width 4)

(setq evil-vsplit-window-right 'left)
(setq evil-split-window-below 'above)

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

;;; Make sure that pressing the escape key on its own will
;;; be translated to 'escape if no further event arrives.
;;; This should happen everywhere, not just in evil-mode buffers
;;; Specifically this should happen in minibuffers (I think that's
;;; where I mean) as well.
;;; Might be some wierdness though so bear in mind.
(defvar my-esc-map nil
  "Original ESC prefix map in `input-decode-map'.
Used by `my-esc-mode'.")

(defun my-init-esc (frame)
  "Update `input-decode-map' in terminal."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (and
             (eq (terminal-live-p term) t)
             (not (terminal-parameter term 'my-esc-map)))
        (let ((my-esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'my-esc-map my-esc-map)
          (define-key input-decode-map [?\e]
            `(menu-item "" ,my-esc-map :filter ,#'my-esc)))))))

(defun my-esc (map)
  "Translate \\e to 'escape if no further event arrives.
This function is used to translate a \\e event either to 'escape
or to the standard ESC prefix translation map. If \\e arrives,
this function waits for `my-esc-delay' seconds for another
event. If no other event arrives, the event is translated to
'escape, otherwise it is translated to the standard ESC prefix
map stored in `input-decode-map'.
The translation to 'escape happens only if the current command
has indeed been triggered by \\e. In other words, this will only
happen when the keymap is accessed from `read-key-sequence'. In
particular, if it is access from `define-key' the returned
mapping will always be the ESC prefix map."
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for 0.01))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(add-hook 'after-make-frame-functions #'my-init-esc)
(mapc #'my-init-esc (frame-list))

(provide 'evil-setup)
