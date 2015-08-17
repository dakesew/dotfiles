;Mixed Stuff, that wouldn't warrant its own file
;;I don't want to write yes everytime
(defalias 'yes-or-no-p 'y-or-n-p)
;;UTF-8 GODDAMIT
(setq locale-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(unless (eq system-type 'windows-nt)
  ; on Win32, cooperation between Emacs and other Unicode applications is weird.
  ; let's avoid that (maybe in the far future).
  (set-selection-coding-system 'utf-8-unix))
(prefer-coding-system 'utf-8-unix)

;; I prefer my backups sorted elsewhere:
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      5  ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old


;; While I'd like Emacs to backup files I am working on, I would prefer them to be stored
;; outside the original directories so they won't pollute my file lists:
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Avoid confusion in the modeline when opening multiple files of the same name:
(require 'uniquify) 
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")
(setq make-backup-files nil)

(use-package hungry-delete
  :ensure hungry-delete
  )
(global-hungry-delete-mode)

(use-package powerline
  :ensure powerline
  )
(powerline-default-theme)

(setq c-default-style "linux"
      c-basic-offset 4)
(provide 'dakesew-mixed)
