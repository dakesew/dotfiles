;;; private/default/+mydired.el -*- lexical-binding: t; -*-

(after! dired
  (require 'recentf)
  ;; Add directories to recentf too
  ;; https://www.emacswiki.org/emacs/recentf-ext.el
  (defun recentf-add-dired-directory ()
    (when (and (stringp dired-directory)
               (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  ;; Because of some reason, evil-collection defines SPC
  ;; This breaks the leader key, so here we disable it again
  (define-key dired-mode-map "e" 'eshell-open))
;; Use integrated lisp ls for maximal compatability
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)
(setq ls-lisp-format-time-list
      '("%Y-%m-%d %H:%M"
        "%Y-%m-%d %H:%M"))
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-string-collate nil)
(dired-async-mode t)
;; So image dired doesn't clutter the .emacs.d directory
(setq image-dired-dir "~./emacs.d/.local/image-dired/")
(setq dired-listing-switches "-AFGh")
(setq find-ls-option '("-exec ls -AdFGhlN {} +" . "-adFGhlN"))
