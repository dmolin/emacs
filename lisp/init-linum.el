(require `linum)
(global-linum-mode t)
(set-face-attribute 'fringe' nil :background "#CCC")
(set-face-attribute 'linum' nil :background "#DDD")
(setq linum-format "%4d ")

(provide `init-linum)
