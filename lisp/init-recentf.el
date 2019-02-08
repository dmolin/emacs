(recentf-mode 1)
(setq-default
 recentf-max-saved-items 30
 recentf-exclude '("/tmp/" "/ssh:"))

(run-at-time nil (* 10 60) 'recentf-save-list)

(provide 'init-recentf)
