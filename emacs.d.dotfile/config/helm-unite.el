;; helm-like-unite

(use-package hydra)

;; Hydras to make helm more like unite
(require 'helm-files)
(require 'helm-buffers)

(defvar helm-buffer-after-init-hook nil)

(defclass helm-source-ffiles (helm-source-sync)
  ((header-name
    :initform (lambda (name) name))
   (init
    :initform (lambda ()
                (setq helm-ff-auto-update-flag
                      helm-ff-auto-update-initial-value)
                (setq helm-ff--auto-update-state
                      helm-ff-auto-update-flag)
                (helm-set-local-variable 'bookmark-make-record-function
                                         #'helm-ff-make-bookmark-record)))
   (candidates :initform 'helm-find-files-get-candidates)
   (filtered-candidate-transformer :initform 'helm-ff-sort-candidates)
   (filter-one-by-one :initform 'helm-ff-filter-candidate-one-by-one)
   (persistent-action :initform 'helm-find-files-persistent-action)
   (persistent-help :initform "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
   (help-message :initform 'helm-ff-help-message)
   (mode-line :initform (list "File(s)" helm-mode-line-string))
   (volatile :initform t)
   (cleanup :initform 'helm-find-files-cleanup)
   (migemo :initform t)
   (nohighlight :initform t)
   (keymap :initform helm-find-files-map)
   (candidate-number-limit :initform 'helm-ff-candidate-number-limit)
   (action-transformer
    :initform 'helm-find-files-action-transformer)
   (action :initform 'helm-find-files-actions)
   (before-init-hook :initform 'helm-find-files-before-init-hook)
   (after-init-hook :initform 'helm-find-files-after-init-hook)))

(defclass helm-source-buffers (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'helm-buffers-list--init)
   (candidates :initform helm-buffers-list-cache)
   (matchplugin :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (resume :initform (lambda ()
                       (run-with-idle-timer
                        0.1 nil (lambda ()
                                  (with-helm-buffer
                                    (helm-force-update))))))
   (keymap :initform helm-buffer-map)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (help-message :initform 'helm-buffer-help-message)
   (after-init-hook :initform 'helm-buffer-after-init-hook)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

(defun helm-ff-directory-files (directory &optional full)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' even on root directories in Windows
systems."
  (setq directory (file-name-as-directory
                   (expand-file-name directory)))
  (let* (file-error
         (ls   (condition-case err
                   (directory-files
                    directory full directory-files-no-dot-files-regexp)
                 ;; Handle file-error from here for Windows
                 ;; because predicates like `file-readable-p' and friends
                 ;; seem broken on emacs for Windows systems (always returns t).
                 ;; This should never be called on GNU/Linux/Unix
                 ;; as the error is properly intercepted in
                 ;; `helm-find-files-get-candidates' by `file-readable-p'.
                 (file-error
                  (prog1
                      (list (format "%s:%s"
                                    (car err)
                                    (mapconcat 'identity (cdr err) " ")))
                    (setq file-error t))))))
    ls))

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

(defun unite-buffers-list ()
  "List buffers but act a bit more like unite."
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources '(helm-source-buffers-list
                   helm-source-ido-virtual-buffers
                   helm-source-buffer-not-found)
        :buffer "*helm buffers*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

(setq helm-find-files-after-init-hook 'helm-like-unite-files/body)
(setq helm-buffer-after-init-hook 'helm-like-unite-buffers/body)
(define-key helm-find-files-map (kbd "<escape>") 'helm-like-unite-files/body)
(define-key helm-buffer-map (kbd "<escape>") 'helm-like-unite-buffers/body)
(setq hydra-is-helpful nil)

(provide 'helm-unite)
