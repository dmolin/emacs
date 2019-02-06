(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
(autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
(autoload 'ffip-show-diff "find-file-in-project" nil t)
(autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
(autoload 'ffip-ivy-resume "find-file-in-project" nil t)

;; remap C-x C-f to find-file-in-project
(global-set-key (kbd "C-x C-d") 'counsel-find-file)
(global-set-key (kbd "C-x C-f") 'find-file-in-project)


(defun my-setup-develop-environment ()
  (interactive)
  ;; well, I'm not interested in concatenated BIG js file or file in dist/
  (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/dist/*'")
  ;; exclude below directories and files
  (setq-local ffip-prune-patterns '("*/.git/*" "*/node_modules/*" "*/.meteor/*", "*/bundler-cache/*", "*/dist/*")))

  (defun ffip--create-exclude-find-options (names)
    (mapconcat (lambda (name)
                 (concat "-not -regex \".*" name ".*\"")) names " "))

  (setq-default ffip-find-options
              (ffip--create-exclude-find-options
               '("/node_modules"
                 ".meteor"
                 ".git"
                 "/bower_components"
                 "/target"
                 "/build"
                 "/dist"
                 "/generated"
                 "/.tmp")))
;; most major modes inherit from prog-mode, so below line is enough
(add-hook 'prog-mode-hook 'my-setup-develop-environment)

(provide 'init-find-file-in-project)
