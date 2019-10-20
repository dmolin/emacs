;;; this file bootstrap the configuration, which is divided into a number of other files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "local-packages" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------

;; - Restore removed var alias, used by ruby-electric-brace and others
(unless (boundp 'last-command-char)
  (defvaralias 'last-command-char 'last-command-event))


(require 'init-utils)
;; must come before elpa, as it may provide package.el
(require 'init-site-lisp)

;; initialize repositories
(require 'init-elpa)
(require 'init-exec-path)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)
;; (require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
;; (require-package 'mwe-log-commands)

(require 'init-frame-hooks)
(require 'init-gui-frames)
(require 'init-xterm)

(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-flycheck)
(require 'init-recentf)
;; sort M-x commands starting from the most recent ones
(require 'init-smex)
(require 'init-ivy)

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'init-windows)
(require 'init-sessions)

(require 'init-fonts)
(require 'init-mmm)
(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)
(require 'init-github)
(require 'init-projectile)
(require 'init-crontab)
(require 'init-markdown)
(require 'init-csv)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)

(require 'init-lisp)
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)
(require 'init-folding)

(when *is-a-mac*
  (require-package 'osx-location))
(require 'regex-tool)
(require-package 'desktop+)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)
;; (require 'init-neotree)

(require 'init-find-file-in-project)
(require 'init-linum)
(require 'init-stylus)
(require 'init-org-bullets-mode)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-web-mode)
(require 'init-themes)
(require 'init-hiwin)
(require 'init-local nil t)

(provide 'init)
