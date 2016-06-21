;;; rumblesan-powerline-el -- Rumblesan Modeline theme

(require 'packages)

(require 'powerline)
(require 'evil)

;;; before requiring powerline-evil
(defface powerline-evil-base-face
  '((t (:foreground "black" :inherit mode-line)))
  "Base face for powerline evil faces."
  :group 'powerline)

(require 'powerline-evil)
(require 'moe-theme)

(moe-theme-set-color 'cyan)
(powerline-moe-theme)


(defun powerline-rumblesan-theme ()
  "Custom modeline"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (evil-face (powerline-evil-face))
                          (evil-mode-line-tag (upcase (concat (symbol-name evil-state))))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (buffer-encoding (let ((buf-coding (format "%s" buffer-file-coding-system)))
                                             (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                                                 (match-string 1 buf-coding)
                                               buf-coding)))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw evil-mode-line-tag evil-face 'l)
                                     (powerline-raw " " evil-face)
                                     (funcall separator-left evil-face mode-line)
                                     (powerline-raw "%*" mode-line 'l)
                                     (powerline-buffer-size mode-line 'l)
                                     (powerline-buffer-id mode-line-buffer-id 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " " mode-line-buffer-id)
                                     (funcall separator-left mode-line face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-raw "|" face1)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw buffer-encoding face1 'l)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " " mode-line)
                                     (powerline-raw "%6p" mode-line 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-rumblesan-theme)

(provide 'rumblesan-powerline)

