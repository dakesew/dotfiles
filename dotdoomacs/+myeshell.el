;;; private/default/+myeshell.el -*- lexical-binding: t; -*-


(add-hook! eshell-mode
  (lambda ()
    (eshell-hist-use-global-history)
    (local-set-key (kbd "M-R") 'eshell-list-history)
    (local-set-key (kbd "M-r")
                   (lambda ()
                     (interactive)
                     (insert
                      (ido-completing-read "Eshell history: "
                                           (delete-dups
                                            (ring-elements eshell-history-ring))))))))

(after! eshell
        (setq eshell-destroy-buffer-when-process-dies t))

(after! em-alias
  ;; Define permanent eshell aliases
  ;; Somehow system sudo is called per default, and it has strange behaviour
  (eshell/alias "sudo" "eshell/sudo $*")
  (eshell/alias "ff" "find-file")
  (eshell/alias "ee" "find-file-other-window")
  (eshell/alias "cover" "wget -O cover.jpg {xclip -o}")
  (eshell/alias "gain" "mp3gain -r ./**/*.mp3"))
(defun eshell/d ()
  (dired "."))

;; Use shared history in eshel
(defvar eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

(defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (let (eshell-history-ring)
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq eshell-history-global-ring eshell-history-ring))
    (unless eshell-history-ring (setq eshell-history-global-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))

(defun eshell-open ()
  (interactive)
  ;; With (eshell t) a new eshell will be opened
  (if (eq major-mode 'eshell-mode)
      (eshell t)
    (let ((cwd default-directory))
      (eshell)
      (if (eshell-process-interact 'process-live-p)
          (message "Won't change CWD because of running process.")
        (setq default-directory cwd)
        (eshell-reset)))))
