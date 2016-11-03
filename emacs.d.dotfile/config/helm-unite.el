;; helm-like-unite

;; Unite like file browsing
(require 'helm-files)

(defun helm-ff-directory-files (directory &optional full)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but never returns the
dotted filename '.' and '..'"
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

(defvar unite-find-files-after-init-hook)
(setq unite-find-files-after-init-hook 'helm-like-unite-files/body)

(defclass helm-source-ffiles (helm-source-sync)
  (
   (candidates :initform 'helm-find-files-get-candidates)
   (filter-one-by-one :initform 'helm-ff-filter-candidate-one-by-one)
   (persistent-action :initform 'helm-find-files-persistent-action)
   (mode-line :initform (list "File(s)" helm-mode-line-string))
   (volatile :initform t)
   (cleanup :initform 'helm-find-files-cleanup)
   (nohighlight :initform t)
   (keymap :initform helm-find-files-map)
   (candidate-number-limit :initform 'helm-ff-candidate-number-limit)
   (action :initform 'helm-find-files-actions) ;
   (after-init-hook :initform 'unite-find-files-after-init-hook)))

(defun unite-find-files ()
  "Find FNAME with `helm' completion.
Like `find-file' but with `helm' support.
Use it for non--interactive calls of `helm-find-files'."
  (interactive)
  
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (let* ((fname (expand-file-name (helm-current-directory)))
         ;; Be sure we don't erase the precedent minibuffer if some.
         (helm-ff-auto-update-initial-value
          (and helm-ff-auto-update-initial-value
               (not (minibuffer-window-active-p (minibuffer-window)))))
         (tap (thing-at-point 'filename))
         (def (and tap (or (file-remote-p tap)
                           (expand-file-name tap)))))
    (set-text-properties 0 (length fname) nil fname)
    (unless helm-source-find-files
      (setq helm-source-find-files (helm-make-source
                                    "Find Files" 'helm-source-ffiles)))
    (mapc (lambda (hook)
            (add-hook 'helm-after-update-hook hook))
          '(helm-ff-move-to-first-real-candidate
            helm-ff-update-when-only-one-matched
            helm-ff-auto-expand-to-home-or-root))
    (unwind-protect
         (helm :sources 'helm-source-find-files
               :input fname
               :case-fold-search t
               :ff-transformer-show-only-basename
               helm-ff-transformer-show-only-basename
               :default def
               :prompt "path: "
               :buffer "*helm find files*")
      (helm-attrset 'resume `(lambda ()
                               (setq helm-ff-default-directory
                                     ,helm-ff-default-directory
                                     helm-ff-last-expanded
                                     ,helm-ff-last-expanded))
                    helm-source-find-files)
      (setq helm-ff-default-directory nil))))

;; Unite like buffer browsing
(require 'helm-buffers)

(defvar unite-buffer-after-init-hook)
(setq unite-buffer-after-init-hook 'helm-like-unite-buffers/body) ;

(defclass helm-source-buffers (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'helm-buffers-list--init)
   (candidates :initform helm-buffers-list-cache)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform helm-buffer-map)
   (volatile :initform t)
   (after-init-hook :initform 'unite-buffer-after-init-hook)
   ))

;; Hydra setup
(use-package hydra)

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
  ("RET" helm-find-file-or-expand)
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
  ("D" helm-buffer-run-kill-persistent)
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

(define-key helm-find-files-map (kbd "<escape>") 'helm-like-unite-files/body)
(define-key helm-buffer-map (kbd "<escape>") 'helm-like-unite-buffers/body)
(setq hydra-is-helpful nil)


(defun helm-find-file-or-expand ()
  (interactive)
  (with-helm-window
    (if (and (file-directory-p (helm-get-selection))
             (< (length (helm-marked-candidates)) 2))
        (helm-execute-persistent-action)
        (helm-exit-minibuffer))))
(define-key helm-find-files-map (kbd "RET") 'helm-find-file-or-expand)

(provide 'helm-unite)
