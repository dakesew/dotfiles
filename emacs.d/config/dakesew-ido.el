(require 'ido)
(ido-mode t)
(ido-everywhere t)
(use-package ido-ubiquitous
  :ensure ido-ubiquitous
  )
(ido-ubiquitous-mode 1)
(use-package smex
  :ensure smex
  )
(smex-initialize)

(provide 'dakesew-ido)
