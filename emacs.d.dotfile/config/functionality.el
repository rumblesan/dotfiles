(require 'packages)

(ensure-package-installed 'helm
                          'hydra
                          'evil
                          'evil-leader
                          'shackle
                          'evil
                          'evil-leader)

(require 'esc-to-escape-setup)
(require 'tmux-navigate)

(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-display-header-line nil)

;; Hydras to make helm more like unite
(require 'helm-files)
(require 'helm-buffers)
(defhydra helm-like-unite-files (:hint nil
                                 :color red)
  "
Nav ^^^^^^^^^      Mark ^^          Other ^^       Quit
^^^^^^------------^^----------------^^----------------------
^ ^ _k_ ^ ^   _<SPC>_ mark       _o_pen         _i_: cancel
_h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
^ ^ _j_ ^ ^     _U_nmark all     _v_iew
^^^^^^
"
  ;; navigation
  ("h" helm-find-files-up-one-level)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("l" helm-execute-persistent-action)
  ;; mark
  ("<SPC>" helm-toggle-visible-mark)
  ("t" helm-toggle-all-marks)
  ("U" helm-unmark-all)
  ;; exit
  ("<escape>" keyboard-escape-quit "" :exit t)
  ("i" nil "cancel")
  ("o" helm-ff-run-switch-other-window)
  ;; rest
  ("H" helm-help)
  ("v" helm-execute-persistent-action)
  )

(defhydra helm-like-unite-buffers (:hint nil
                                   :color red)
  "
Nav ^^^^^^^^^      Mark ^^          Other ^^       Quit
^^^^^^------------^^----------------^^----------------------
^ ^ _k_ ^ ^   _<SPC>_ mark       _o_pen         _i_: cancel
_h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
^ ^ _j_ ^ ^     _U_nmark all     _v_iew
^^^^^^^^                         _D_elete
"
  ;; navigation
  ("h" helm-find-files-up-one-level)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("l" helm-execute-persistent-action)
  ;; mark
  ("<SPC>" helm-toggle-visible-mark)
  ("t" helm-toggle-all-marks)
  ("U" helm-unmark-all)
  ;; exit
  ("<escape>" keyboard-escape-quit "" :exit t)
  ("i" nil "cancel")
  ("o" helm-buffer-switch-other-window)
  ;; rest
  ("H" helm-help)
  ("v" helm-execute-persistent-action)
  ("D" helm-buffer-run-kill-buffers)
  )

(define-key helm-find-files-map (kbd "<escape>") 'helm-like-unite-files/body)
(define-key helm-buffer-map (kbd "<escape>") 'helm-like-unite-buffers/body)
(setq hydra-is-helpful nil)

;; Keep helm window in check
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align 'below :size 0.4)))
(setq helm-split-window-preferred-function 'ignore)
(shackle-mode)

;; Load evil-leader before evil so it works in all buffers
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'helm-buffers-list
  )

;; Setup evil
(require 'evil)

(define-key evil-motion-state-map ";" 'evil-ex)

;; General niceties
(setq evil-shift-width 4)
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

(provide 'functionality)
