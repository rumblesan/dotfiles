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
(require 'helm-config)
(setq helm-display-header-line nil)
(setq helm-mode-line-string "")

(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; Hydras to make helm more like unite
(require 'helm-files)
(require 'helm-buffers)

(defun helm-buffers-open-other-window (_candidate split-dir)
  "Keep current-buffer and open files in separate vertical windows."
  (let* ((buffers (helm-marked-candidates)))
    (mapc (lambda (b)
            (let ((window (if (eq split-dir 'vertical)
                              (split-window-right)
                              (split-window-below))))
                (select-window window)
                (switch-to-buffer b)))
            buffers)
    (balance-windows)
  )
)

(defun helm-find-files-open-other-window (_candidate split-dir)
  "Keep current-buffer and open files in separate vertical windows."
  (let* ((files (helm-marked-candidates))
         (buffers (mapcar 'find-file-noselect files)))
    (mapc (lambda (b)
            (let ((window (if (eq split-dir 'vertical)
                              (split-window-right)
                              (split-window-below))))
                (select-window window)
                (switch-to-buffer b)))
            buffers)
    (balance-windows)
  )
)

(defun helm-ff-run-switch-other-vertical-window ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (_candidate)
                                     (helm-find-files-open-other-window _candidate 'vertical)))))

(defun helm-ff-run-switch-other-horizontal-window ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (_candidate)
                                     (helm-find-files-open-other-window _candidate 'horizontal)))))

(defun helm-buffer-run-switch-other-vertical-window ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (_candidate)
                                     (helm-buffers-open-other-window _candidate 'vertical)))))

(defun helm-buffer-run-switch-other-horizontal-window ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (_candidate)
                                     (helm-buffers-open-other-window _candidate 'horizontal)))))


(defhydra helm-like-unite-files (:hint nil
                                 :color red)
  "
Nav ^^^^^^^^^      Mark ^^          Other ^^       Quit
^^^^^^------------^^----------------^^----------------------
^ ^ _k_ ^ ^   _<SPC>_ mark       _o_pen hor    _i_: cancel
_h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
^ ^ _j_ ^ ^     _U_nmark all     _v_ertical
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
  ("o" helm-ff-run-switch-other-horizontal-window)
  ("v" helm-ff-run-switch-other-vertical-window)
  ;; rest
  ("H" helm-help)
  )

(defhydra helm-like-unite-buffers (:hint nil
                                   :color red)
  "
Nav ^^^^^^^^^      Mark ^^          Other ^^       Quit
^^^^^^------------^^----------------^^----------------------
^ ^ _k_ ^ ^   _<SPC>_ mark       _o_pen hor     _i_: cancel
_h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
^ ^ _j_ ^ ^     _U_nmark all     _v_ertical
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
  ("o" helm-buffer-run-switch-other-horizontal-window)
  ("v" helm-buffer-run-switch-other-vertical-window)
  ;; rest
  ("H" helm-help)
  ("D" helm-buffer-run-kill-buffers)
  )

(define-key helm-find-files-map (kbd "<escape>") 'helm-like-unite-files/body)
(define-key helm-buffer-map (kbd "<escape>") 'helm-like-unite-buffers/body)
(setq hydra-is-helpful nil)

(require 'shackle)
;; Keep helm window in check
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align 'below :size 0.4)))
(setq helm-split-window-preferred-function 'ignore)
(shackle-mode)

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

;; Load evil-leader before evil so it works in all buffers
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'find-file
  "b" 'list-buffers
  "y" 'pbcopy
  "p" 'pbpaste
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

(provide 'navigation)
