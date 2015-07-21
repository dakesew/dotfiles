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
(evil-leader/set-key "bb" 'helm-buffers-list)
(evil-leader/set-key "bn" 'next-buffer)
(evil-leader/set-key "bp" 'previous-buffer)
(evil-leader/set-key "bl" 'next-buffer)
(evil-leader/set-key "bh" 'previous-buffer)
(evil-leader/set-key "bk" 'kill-buffer)
;;;;Windows
(evil-leader/set-key "wo" 'other-window)
(evil-leader/set-key "w0" 'delete-window)
(evil-leader/set-key "w1" 'delete-other-windows)
(evil-leader/set-key "w2" 'split-window-below)
(evil-leader/set-key "w3" 'split-window-right)
(evil-leader/set-key "w-" 'split-window-below)
(evil-leader/set-key "w/" 'split-window-right)
;;;;compile
(evil-leader/set-key "cc" 'compile)
;;;;Terminal
(evil-leader/set-key "tt" 'multi-term)
(evil-leader/set-key "tn" 'multi-term-next)
(evil-leader/set-key "tp" 'multi-term-prev)
(evil-leader/set-key "tl" 'multi-term-next)
(evil-leader/set-key "th" 'multi-term-prev)
;;;;Lisp
(evil-leader/set-key "leb" 'eval-buffer)
(evil-leader/set-key "lee" 'eval-expression)
;;;;Single Key Actions
(evil-leader/set-key "q" 'kill-buffer-and-window)
(evil-leader/set-key ":" 'execute-extended-command)
(evil-leader/set-key "!" 'shell-command)

(provide 'dakesew-evil-leader)
