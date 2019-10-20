;; (require-package 'color-theme-sanityinc-solarized)
;; (require-package 'color-theme-sanityinc-tomorrow)

;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(sanityinc-solarized-light))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'hickey t)
;;(setq-default custom-enabled-themes '(hickey))
;; (load-theme 'zenburn t)
;; (setq-default custom-enabled-themes '(zenburn))

(setq alect-display-class '((class color) (min-colors 256)))

(load-theme 'sanityinc-tomorrow-blue)
(setq-default custom-enabled-themes '(sanityinc-tomorrow-blue))


;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (color-theme-sanityinc-solarized-light))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (color-theme-sanityinc-solarized-dark))

(defun theme-dark ()
  "Activate a cool dark theme."
  (interactive)
  (hickey))

(provide 'init-themes)
