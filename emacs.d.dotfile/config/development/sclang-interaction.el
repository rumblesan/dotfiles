
(require 'evil-leader)
(evil-leader/set-key-for-mode 'sclang-mode
  "v" 'backward-sexp
  "x" 'sclang-eval-region
  "e" 'sclang-eval-defun
  )

(provide 'sclang-interaction)
