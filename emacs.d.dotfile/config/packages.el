;;; packages.el --- Useful stuff for setting up packages

(require 'package)

(package-initialize)

;; Detect online status, from ESK
(require 'cl)
(defun esk-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

(dolist (source '(("org" . "http://orgmode.org/elpa")
                  ("gnu" . "https://elpa.gnu.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
  Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
	 nil
	   (package-install package)
       ))
   packages))
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents)))

(provide 'packages)
