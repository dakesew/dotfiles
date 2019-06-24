;;; private/default/+mydired.el -*- lexical-binding: t; -*-

(after! dired
  (require 'recentf)
  ;; Add directories to recentf too
  ;; https://www.emacswiki.org/emacs/recentf-ext.el
  (defun recentf-add-dired-directory ()
    (when (and (stringp dired-directory)
               (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory))

;; ;; Use integrated lisp ls for maximal compatability
;; (require 'ls-lisp)
;; (setq ls-lisp-use-insert-directory-program nil
;;       ls-lisp-format-time-list
;;       '("%Y-%m-%d %H:%M"
;;         "%Y-%m-%d %H:%M")
;;       ls-lisp-dirs-first t
;;       ls-lisp-use-string-collate nil)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode t)
(remove-hook 'dired-after-readin-hook #'+dired|sort-directories-first)
(remove-hook 'dired-initial-position-hook #'dired-k)
(remove-hook 'dired-after-readin-hook #'dired-k-no-revert)
;; So image dired doesn't clutter the .emacs.d directory
(setq ;dired-listing-switches "-lFaGh1v --group-directories-first"
      find-ls-option '("-exec ls -AdFGhlN {} +" . "-adFGhlN")
      dired-dwim-target 't)

(map! :after dired
      :map dired-mode-map
      :n "<delete>" #'dired-find-file
      :n "<backspace>" #'dired-up-directory)
