;;; private/default/+myeshell.el -*- lexical-binding: t; -*-

(def-package! eshell ; built-in
  :commands eshell-mode
  :init
  (setq eshell-directory-name (concat doom-etc-dir "/eshell")
        eshell-scroll-to-bottom-on-input 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t
        ;; Use shared history in eshel
        eshell-history-global-ring nil
        ;; em-alias
        eshell-aliases-file (concat doom-local-dir ".eshell-aliases"))

  :config
  (require 'em-tramp)
  (set! :evil-state 'eshell-mode 'insert)

  (after! em-term
    ;; Visual commands require a proper terminal. Eshell can't handle that, so it
    ;; delegates these commands to a term buffer.
    (nconc eshell-visual-commands '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim"))
    (setq eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))))

  ;; Aliases
  (setq eshell-command-aliases-list
        '(("q"   "exit")
          ("l"   "ls -1")
          ("ll"  "ls -l")
          ("la"  "ls -la")
          ("sudo" "eshell/sudo")
          ("ff" "find-file")
          ("ee" "find-file-other-window")
          ("cover" "wget -O cover.jpg {xclip -o}")
          ("gain" "mp3gain -r ./**/*.mp3"))))


(add-hook! eshell-mode
  (lambda ()
    (eshell-hist-use-global-history)
    (local-set-key (kbd "M-R") 'eshell-list-history)
    (local-set-key (kbd "M-r") 'counsel-esh-history)))

(defun eshell/d ()
  (dired "."))

(defun eshell-open ()
  "Open a new eshell"
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

(defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (let (eshell-history-ring)
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq eshell-history-global-ring eshell-history-ring))
    (unless eshell-history-ring (setq eshell-history-global-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))

(defun +eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize (+eshell--current-git-branch) 'face 'font-lock-function-name-face)
          (propertize " λ " 'face 'font-lock-constant-face)))

(defun +eshell--current-git-branch ()
    (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))
