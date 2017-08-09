
(defun setup-lisp-interaction-mode ()
  (message "lisp mode")
  (general-nvmap
   :prefix ","
   "e" 'eval-print-last-sexp
   "x" 'eval-last-sexp
   ))

(add-hook 'lisp-interaction-mode-hook #'setup-lisp-interaction-mode)

(provide 'lisp-interaction)
