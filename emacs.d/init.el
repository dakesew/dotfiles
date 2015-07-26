(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

;;Seperate files
(require 'dakesew-theme)
(require 'dakesew-astethics)
(require 'dakesew-mixed)
;;;Evil-Leader needs to be run before evil
(require 'dakesew-evil-leader)
(require 'dakesew-evil)
(require 'dakesew-ido)
(require 'dakesew-auto-complete)

(use-package 2048-game
  :ensure 2048-game
  )
(use-package multi-term
  :ensure multi-term
  )
(setq multi-term-program "/bin/zsh")
(use-package tex-site
  :ensure auctex
  )
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea" default)))
 '(ede-project-directories
   (quote
    ("/home/david/Documents/private/electronics/meet_the_STM32F030/OneOffs/stm32f030f4-Projects/Blink_Led"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
