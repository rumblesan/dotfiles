(ensure-package-installed 'helm)
(require 'helm-config)


(ensure-package-installed 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)

(helm-mode 1)

(provide 'helm-setup)
