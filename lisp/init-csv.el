(require-package 'csv-mode)
(require 'csv-nav)

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
