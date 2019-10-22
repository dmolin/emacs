;; Code:
(add-to-list 'load-path (expand-file-name "local-packages" user-emacs-directory))

(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

(require 'package)
(add-to-list 'package-archives `("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives `("melpa" . "https://melpa.org/packages/"))

;; NOTE: In case of MELPA problems, the official mirror URL is
;; https://www.mirrorservice.org/sites/stable.melpa.org/packages/
(setq package-enable-at-startup nil)
(package-initialize)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(org-babel-load-file (concat user-emacs-directory "org-init.org"))

(provide 'init)
;;; init.el ends here
