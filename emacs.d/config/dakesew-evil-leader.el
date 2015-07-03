;;;Evil-Leader
(use-package evil-leader
  :ensure evil-leader
  )
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
;;;;Files
(evil-leader/set-key "fw" 'save-buffer)
(evil-leader/set-key "fe" 'find-file)
;;;;Buffer
(evil-leader/set-key "bq" 'kill-buffer)
(evil-leader/set-key "bl" 'helm-buffers-list)
;;;;compile
(evil-leader/set-key "cc" 'compile)
;;;;Terminal
(evil-leader/set-key "tt" 'multi-term)
(evil-leader/set-key "tn" 'multi-term-next)
(evil-leader/set-key "tp" 'multi-term-prev)
(evil-leader/set-key "tl" 'multi-term-next)
(evil-leader/set-key "th" 'multi-term-prev)
;;;;Single Key Actions
(evil-leader/set-key "q" 'kill-buffer-and-window)
(evil-leader/set-key ":" 'execute-extended-command)
(evil-leader/set-key "!" 'shell-command)
;;;;Lisp
(evil-leader/set-key "leb" 'eval-buffer)
(evil-leader/set-key "lee" 'eval-expression)

(provide 'dakesew-evil-leader)
