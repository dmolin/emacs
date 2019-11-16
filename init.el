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

(require 'exec-path-from-shell)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.bookmarks.el")
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-blue)))
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(package-selected-packages
   (quote
    (ponylang-mode writeroom-mode org json-mode web-mode origami org-cliplink github-issues yagist whole-line-or-region whitespace-cleanup-mode wgrep unfill undo-tree tagedit switch-window smex skewer-less session scss-mode scratch sass-mode rainbow-mode rainbow-delimiters projectile page-break-lines org-pomodoro nlinum multiple-cursors move-dup mmm-mode markdown-mode magit-gh-pulls macrostep lively indent-guide highlight-symbol highlight-quoted highlight-escape-sequences guide-key gitignore-mode github-clone gitconfig-mode git-timemachine git-messenger fullframe flycheck-package expand-region exec-path-from-shell elisp-slime-nav disable-mouse diminish diff-hl default-text-scale csv-mode counsel cl-lib-highlight cask-mode bug-reference-github browse-kill-ring browse-at-remote avy auto-compile anzu aggressive-indent color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized)))
 '(safe-local-variable-values (quote ((no-byte-compile t))))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(writeroom-mode-line t)
 '(writeroom-restore-window-config t)
 '(writeroom-width 160))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 110 :width normal :family "Iosevka SS09"))))
 '(trailing-whitespace ((t (:background "#07182E" :foreground "#ffeead")))))


(org-babel-load-file (concat user-emacs-directory "org-init.org"))

(provide 'init)
;;; init.el ends here
