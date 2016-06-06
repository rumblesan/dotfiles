(setq my-lisp-dir
    (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path my-lisp-dir)

(setq make-backup-files nil)

;;; Turn bell off entirely
(setq ring-bell-function 'ignore)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;;; Mouse settings
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

(when (eq system-type 'darwin)
  (require 'osx-tweaks)
  )

(require 'packages)

(require 'display-setup)

(require 'evil-setup)
(require 'tmux-navigate)

(ensure-package-installed 'helm)
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

(ensure-package-installed 'hydra)

(require 'helm-files)
(require 'helm-buffers)

(defhydra helm-like-unite-files (:hint nil
                                 :color pink)
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
                                 :color pink)
  "
Nav ^^^^^^^^^      Mark ^^          Other ^^       Quit
^^^^^^------------^^----------------^^----------------------
^ ^ _k_ ^ ^   _<SPC>_ mark       _o_pen         _i_: cancel
_h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
^ ^ _j_ ^ ^     _U_nmark all     _v_iew
^^^^^^                         _D_elete
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

(ensure-package-installed 'shackle)
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align 'below :size 0.4)))
(setq helm-split-window-preferred-function 'ignore)
(shackle-mode)
