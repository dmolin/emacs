(require-package 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;; use eslint with web-mode
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint `web-mode)
  (flycheck-add-mode 'javascript-eslint `js2-mode))

;; customize flycheck temp prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; use local (project specific) eslint whenever possible
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(provide 'init-flycheck)
