(when (display-graphic-p)
  (use-package minimap
    :ensure minimap
    ))
(minimap-mode)
(setq minimap-width-fraction 0.05)
(setq minimap-window-location 'right)

(provide 'dakesew-minimap)
