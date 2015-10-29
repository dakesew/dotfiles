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
(evil-leader/set-key "bb" 'ido-switch-buffer)
(evil-leader/set-key "bn" 'next-buffer)
(evil-leader/set-key "bp" 'previous-buffer)
(evil-leader/set-key "bl" 'next-buffer)
(evil-leader/set-key "bh" 'previous-buffer)
(evil-leader/set-key "bk" 'kill-buffer)
;;;;Windows
(evil-leader/set-key "wo" 'other-window)
(evil-leader/set-key "wh" 'evil-window-left)
(evil-leader/set-key "wj" 'evil-window-down)
(evil-leader/set-key "wk" 'evil-window-up)
(evil-leader/set-key "wl" 'evil-window-right)
(evil-leader/set-key "w0" 'delete-window)
(evil-leader/set-key "wd" 'delete-window)
(evil-leader/set-key "w1" 'delete-other-windows)
(evil-leader/set-key "w2" 'split-window-below)
(evil-leader/set-key "w3" 'split-window-right)
(evil-leader/set-key "w-" 'split-window-below)
(evil-leader/set-key "w/" 'split-window-right)
(evil-leader/set-key "wrh" 'shrink-window-horizontally)
(evil-leader/set-key "wrl" 'enlarge-window-horizontally)
(evil-leader/set-key "wrj" 'enlarge-window)
(evil-leader/set-key "wrk" 'shrink-window)
;;;;;Magit/Git
(evil-leader/set-key "gm" 'magit-status)
;;;;;Lisp
(evil-leader/set-key "leb" 'eval-buffer)
(evil-leader/set-key "lee" 'eval-expression)
;;;compile
(evil-leader/set-key "cc" 'compile)
(evil-leader/set-key "cf" 'make-flash)
(evil-leader/set-key "cm" 'make-pure)
(evil-leader/set-key "cC" 'make-clean)
(defun make-flash () (interactive) (compile "make flash"))
(defun make-pure  () (interactive) (compile "make"))
(defun make-clean () (interactive) (compile "make clean"))
;;;;Terminal
(evil-leader/set-key "tt" 'multi-term)
(evil-leader/set-key "tn" 'multi-term-next)
(evil-leader/set-key "tp" 'multi-term-prev)
(evil-leader/set-key "tl" 'multi-term-next)
(evil-leader/set-key "th" 'multi-term-prev)
(evil-leader/set-key "t <SPC>" 'multi-term-dedicated-toggle)
;;;;Games
(evil-leader/set-key "gt" 'tetris)
(evil-leader/set-key "g2" '2048-game)
;;;;Various Toggles
(evil-leader/set-key "Tl" 'linum-relative-toggle)
;;;Single Key Actions
(evil-leader/set-key "d" 'kill-buffer-and-window)
(evil-leader/set-key ":" 'smex)
(evil-leader/set-key "!" 'shell-command)

(provide 'dakesew-evil-leader)
