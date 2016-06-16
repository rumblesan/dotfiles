
(require 'evil-leader)
(evil-leader/set-key-for-mode 'lisp-interaction-mode
  "e" 'eval-print-last-sexp
  )

(provide 'lisp-interaction)
