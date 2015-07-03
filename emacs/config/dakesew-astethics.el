;; Astethics
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode 0)
(setq show-trailing-whitespace t
      scroll-bar-mode nil
      make-pointer-invisible nil
      mouse-avoidance-mode "exile")
(visual-line-mode 1)
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-7"))
;;; No, I don't need to know that this is emacs
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(provide 'dakesew-astethics)
