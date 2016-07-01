
(require 'evil-leader)
(evil-leader/set-key-for-mode 'lisp-interaction-mode
  "e" 'eval-print-last-sexp
  "x" 'eval-last-sexp
  )

(provide 'lisp-interaction)
