;;; ~/dotfiles/doom.d/+bindings.el -*- lexical-binding: t; -*-


(map!
 (:leader
   :desc "Eshell Command" :n "!" #'eshell-command
   (:prefix "f"
     :desc "Find File" :n "f" #'counsel-find-file)
   (:prefix "w"
     :desc "Delete window"    :n "c" #'delete-window
     :desc "Maxmimize window" :n "m" #'delete-other-windows)))
(after! evil-org
  (map! :map evil-org-mode-map
        (:localleader
          :n "m" #'org-toggle-latex-fragment)))
