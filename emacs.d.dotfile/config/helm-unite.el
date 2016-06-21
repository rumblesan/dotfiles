;; helm-like-unite

(use-package hydra)

;; Hydras to make helm more like unite
(require 'helm-files)
(require 'helm-buffers)

(defun unite-open-buffer-other-window (active-window split-dir)
  "Open a buffer in a new window"
  (lambda (candidate)
    (select-window (split-window active-window nil
                                 (if (eq split-dir 'vertical) 'right 'below)))
    (switch-to-buffer (cond ((bufferp candidate) candidate)
                            ((file-regular-p candidate) (find-file-noselect candidate))))))

(defun unite-candidates-open-other-window (_candidate split-dir)
  "Keep current-buffer and open files in separate vertical windows."
  (let* ((candidates (helm-marked-candidates))
         (active-window (selected-window)))
    (mapc (unite-open-buffer-other-window active-window split-dir) candidates)
    (balance-windows (window-parent active-window))
  )
)

(defun helm-candidates-run-switch-other-vertical-window ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (_candidate)
                                     (unite-candidates-open-other-window _candidate 'vertical)))))

(defun helm-candidates-run-switch-other-horizontal-window ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (_candidate)
                                     (unite-candidates-open-other-window _candidate 'horizontal)))))


(defhydra helm-like-unite-files (:hint nil
                                 :color red)
  "
Nav ^^^^^^^^^      Mark ^^          Other ^^       Quit
^^^^^^------------^^----------------^^----------------------
^ ^ _k_ ^ ^   _<SPC>_ mark       _s_plit         _i_: cancel
_h_ ^✜^ _l_     _t_oggle mark    _H_elp
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
  ("s" helm-candidates-run-switch-other-horizontal-window)
  ("v" helm-candidates-run-switch-other-vertical-window)
  ;; rest
  ("H" helm-help)
  )

(defhydra helm-like-unite-buffers (:hint nil
                                   :color red)
  "
Nav ^^^^^^^^^      Mark ^^          Other ^^       Quit
^^^^^^------------^^----------------^^----------------------
^ ^ _k_ ^ ^   _<SPC>_ mark       _s_plit          _i_: cancel
_h_ ^✜^ _l_     _t_oggle mark    _H_elp
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
  ("s" helm-candidates-run-switch-other-horizontal-window)
  ("v" helm-candidates-run-switch-other-vertical-window)
  ;; rest
  ("H" helm-help)
  ("D" helm-buffer-run-kill-buffers)
  )

(define-key helm-find-files-map (kbd "<escape>") 'helm-like-unite-files/body)
(define-key helm-buffer-map (kbd "<escape>") 'helm-like-unite-buffers/body)
(setq hydra-is-helpful nil)

(provide 'helm-unite)
