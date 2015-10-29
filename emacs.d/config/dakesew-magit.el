(use-package magit
  :ensure magit
  )

(setq magit-completing-read-function
      'magit-ido-completing-read)

(provide 'dakesew-magit)
