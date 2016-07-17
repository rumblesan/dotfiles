(require 'generic-x)

(defface pegjs-operator-face
  '((((class color) (min-colors 88) (background light)) :foreground "Blue1")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 16) (background light)) :foreground "Blue")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 8)) :foreground "blue" :weight bold)
    (t :inverse-video t :weight bold))
  "PegJS mode face used to highlight operators."
  :group 'pegjs-faces)

(define-generic-mode
  'pegjs-mode
  '("#" ("/*" . "*/"))
  '("function" "return" "var")
  '((";" . 'font-lock-builtin-face)
    ("=" . 'pegjs-operator-face)
    ("?" . 'pegjs-operator-face)
    ("*" . 'pegjs-operator-face)
    ("^[A-Z][a-zA-Z0-9]+" . 'font-lock-type-face)
    ("[a-z][a-zA-Z0-9]+:" . 'font-lock-variable-name-face)
    )
  '("\\.pegjs$")
  nil
  "A mode for pegjs files"
)

(provide 'pegjs-mode)
