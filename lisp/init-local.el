;; ======================================================
;; INIT LOCAL
;; ======================================================

(add-hook 'before-save-hook 'delete-trailing-whitespace)
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
(require 'buffer-move)
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

;; window dedication
(defun dedicated-window-toggle ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;; desktop
(desktop-save-mode 0)

(provide 'init-local)
