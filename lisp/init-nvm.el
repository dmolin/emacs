(require 'nvm)
(nvm-use (caar (last (nvm--installed-versions))))

(provide 'init-nvm)
