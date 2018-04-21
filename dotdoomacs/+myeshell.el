;;; private/default/+myeshell.el -*- lexical-binding: t; -*-
;; Parts adapted from https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-eshell.el

(def-package! eshell ; built-in
  :commands eshell-mode
  :init
  (setq eshell-directory-name (concat doom-etc-dir "/eshell")
        eshell-scroll-to-bottom-on-input 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        ;; em-prompt
        eshell-prompt-regexp "^λ "
        eshell-prompt-function #'+eshell-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t
        ;; Use shared history in eshell
        eshell-history-global-ring nil
        eshell-hist-ignoredups t
        eshell-input-filter
        (lambda (str)
          (not (or
                ;; Here we can filter out failing commands.  This is usually a bad
                ;; idea since a lot of useful commands have non-zero exit codes
                ;; (including Emacs/Eshell functions).
                ;; (/= eshell-last-command-status 0)
                (string= "" str)
                (string-prefix-p " " str))))

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
    (when eshell-history-file-name
      (eshell-read-history nil t))
    (setq eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))

(defun +eshell--current-git-branch ()
  (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                           when (string-match "^\*" match)
                           collect match))))
    (if (not (eq branch nil))
        (concat " [" (substring branch 2) "]")
      "")))

(defun +eshell-prompt ()
  (let ((path (abbreviate-file-name (eshell/pwd))))
    (concat
     (when eshell-status-p
       (propertize (or (eshell-status-display) "") 'face font-lock-comment-face))
     (format
      (propertize "(%s@%s)" 'face '(:weight bold))
      (propertize (user-login-name) 'face '(:foreground "cyan"))
      (propertize (system-name) 'face '(:foreground "cyan")))
     (if (and (require 'magit nil t) (or (magit-get-current-branch) (magit-get-current-tag)))
         (let* ((root (abbreviate-file-name (magit-rev-parse "--show-toplevel")))
                (after-root (substring-no-properties path (min (length path) (1+ (length root))))))
           (format
            (propertize "[%s/%s@%s]" 'face '(:weight bold))
            (propertize root 'face `(:foreground ,(if (= (user-uid) 0) "orange" "gold")))
            (propertize after-root 'face `(:foreground ,(if (= (user-uid) 0) "red" "green") :weight bold))
            (or (magit-get-current-branch) (magit-get-current-tag))))
       (format
        (propertize "[%s]" 'face '(:weight bold))
        (propertize path 'face `(:foreground ,(if (= (user-uid) 0) "red" "green") :weight bold))))
     (propertize "\nλ" 'face '(:weight bold))
     " ")))


;;; Extra execution information
(defvar eshell-status-p t
  "If non-nil, display status before prompt.")
(defvar eshell-status--last-command-time nil)
(make-variable-buffer-local 'eshell-status--last-command-time)
(defvar eshell-status-min-duration-before-display 1
  "If a command takes more time than this, display its duration.")

(defun eshell-status-display ()
  (when eshell-status--last-command-time
    (let ((duration (time-subtract (current-time) eshell-status--last-command-time)))
      (setq eshell-status--last-command-time nil)
      (when (> (time-to-seconds duration) eshell-status-min-duration-before-display)
        (format "#[STATUS] End time %s, duration %.3fs\n"
                (format-time-string "%F %T" (current-time))
                (time-to-seconds duration))))))


(defun eshell-status-record ()
  (setq eshell-status--last-command-time (current-time)))

(add-hook 'eshell-pre-command-hook 'eshell-status-record)
