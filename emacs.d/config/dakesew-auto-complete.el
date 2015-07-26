(use-package auto-complete
  :ensure auto-complete
  )
(use-package auto-complete-config)
(ac-config-default)

(semantic-mode 1)
(global-ede-mode 1)
; let's define a function which adds semantic as a suggestion backend to auto complete
; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(provide 'dakesew-auto-complete)
