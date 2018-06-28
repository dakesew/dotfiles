;;; ~/dotfiles/doom.d/+bindings.el -*- lexical-binding: t; -*-


(map!
 (:leader
   :desc "Eshell Command" :n "!" #'eshell-command
   (:prefix "f"
     :desc "Find File" :n "f" #'counsel-find-file)
   (:prefix "w"
     :desc "Maxmimize window" :n "m" #'delete-other-windows)))
