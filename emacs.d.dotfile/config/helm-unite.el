;; helm-like-unite

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
  ("s" helm-ff-run-switch-other-horizontal-window)
  ("v" helm-ff-run-switch-other-vertical-window)
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
  ("s" helm-buffer-run-switch-other-horizontal-window)
  ("v" helm-buffer-run-switch-other-vertical-window)
  ;; rest
  ("H" helm-help)
  ("D" helm-buffer-run-kill-buffers)
  )

(define-key helm-find-files-map (kbd "<escape>") 'helm-like-unite-files/body)
(define-key helm-buffer-map (kbd "<escape>") 'helm-like-unite-buffers/body)
(setq hydra-is-helpful nil)

(provide 'helm-unite)
