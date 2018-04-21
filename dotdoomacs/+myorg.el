;;; private/default/+myorg.el -*- lexical-binding: t; -*-


;; ==== org general settings {{{ ====
(setq org-directory "~/usr/org"
      org-default-notes-file "~/usr/org/notes.org"
      org-archive-location "~/usr/org/notes.org::* Archived"
      org-hide-emphasis-markers t
      org-startup-indented t
      org-html-validation-link nil
      org-startup-with-inline-images t
      org-highlight-latex-and-related '(latex script entities))
(setq org-image-actual-width (quote (500)))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *-.*\\( ::\\) "
                           (0 (prog1 () (compose-region (match-beginning 1)
                                                        (match-end 1) ""))))))
;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook
          'org-display-inline-images 'append)

(after! ob-plantuml
    (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(after! org-mode
  ;; Set inline image size
  (setq org-image-actual-width (/ (display-pixel-width) 4))
  ;; Display pdfs inline https://stackoverflow.com/a/35261577
  (add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
  (add-to-list 'image-file-name-extensions "pdf")
  (setq imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))
  ;; EPS too
  (add-to-list 'image-type-file-name-regexps '("\\.eps\\'" . imagemagick)  )
  (add-to-list 'image-file-name-extensions "eps")
  ;; Define a custom link type to reference dead paper
  (org-add-link-type "paper" 'org-paper-open)
  (defun org-paper-open (path)
    (message path)))
(after! ox-latex
  ;; Use xelatex for unicode support
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f")))
(after! org-src
  ;; When editing a code snippet, use the current window rather than popping
  ;; open a new one (which shows the same information)
  (setq org-src-window-setup 'current-window))

(after! org
    (setq org-agenda-files (list (expand-file-name "~/usr/log/agenda.org"))))
