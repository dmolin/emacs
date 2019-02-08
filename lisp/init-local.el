;; ======================================================
;; CUSTOMIZATIONS
;; ======================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 ;;'(display-time-mode t)
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
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "PfEd" :family "Inconsolata")))))
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "PfEd" :family "Droid Sans Mono")))))


;; ======================================================
;; INIT LOCAL
;; ======================================================

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(desktop-save-mode 0)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(menu-bar-mode -99)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1
      scroll-conservatively 10000) ;; keyboard scroll one line at a time
(setq scroll-preserve-screen-position 't)

;; swap windows buffers easily: Meta (my Ctrl key) + Windows key + arrows
(global-set-key (kbd "<M-s-up>")     'buf-move-up)
(global-set-key (kbd "<M-s-down>")   'buf-move-down)
(global-set-key (kbd "<M-s-left>")   'buf-move-left)
(global-set-key (kbd "<M-s-right>")  'buf-move-right)

(global-set-key (kbd "<s-right>") 'forward-word)
(global-set-key (kbd "<s-left>") 'backward-word)

;; ==========================================
;; SCROLLING
;; =========================================
(defun gcm-scroll-down ()
      (interactive)
      (scroll-up 2))
(defun gcm-scroll-up ()
      (interactive)
      (scroll-down 2))
(global-set-key (kbd "M-s-k") 'gcm-scroll-up)
(global-set-key (kbd "M-s-j") 'gcm-scroll-down)
(global-set-key (kbd "<s-down>") 'gcm-scroll-down)
(global-set-key (kbd "<s-up>") 'gcm-scroll-up)

(global-set-key (kbd "C-c ]") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c [") 'shrink-window-horizontally)
(global-set-key (kbd "C-c #") 'enlarge-window)
(global-set-key (kbd "C-c '") 'shrink-window)

;; ============================
;; TIME DISPLAY
;; ============================

(display-time)
  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (setq mode-line-end-spaces
        (list (propertize " " 'display '(space :align-to (- right 12)))
              'display-time-string))


;; JSON settings
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; handle copy/paste to clipboard. Especially useful when using Emacs from terminal
(setq *is-a-mac* (eq system-type 'darwin))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(defun cc ()
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p) x-select-enable-clipboard)
          (x-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end))))
         (t (shell-command-on-region (region-beginning) (region-end)
                                     (cond
                                      (*cygwin* "putclip")
                                      (*is-a-mac* "pbcopy")
                                      (*linux* "xsel -ib")))
            ))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
        (message "No region active; can't yank to clipboard!")))

(global-set-key (kbd "C-<S-C>") 'cc)

;; projectile customisations
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x d") 'projectile-find-dir)

;; we're using ivy-switch-buffers along with recentf to do the following
;; (global-set-key (kbd "C-x b") 'projectile-switch-to-buffer)

;; force .js files too as web-mode
(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))

;; customise fci (fill-column-indicator)
;;(setq fci-rule-column 100)
;;(global-fci-mode 1)

;; increase flycheck error limits to avoid disabling the linter
(setq flycheck-checker-error-threshold 10000)

;; ORG-MODE AGENDAS

(setq org-agenda-files (list "~/tresors/private/org/piano.org"
                             "~/tresors/private/org/hubro.org"
                             "~/tresors/private/org/home.org"))

;; MAGIT

(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window))))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; column indicator
(setq-default
 whitespace-line-column 100
 whitespace-style       '(face lines-tail))

(add-hook 'prog-mode-hook #'whitespace-mode)

(provide 'init-local)
