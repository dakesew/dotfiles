(defconst my-org-packages
  '(ox-reveal zpresent org-page org))

(defun my-org/pre-init-org ()
  ;; Disable emphasis markers (it's obvious that markers there and hiding
  ;; them cleans up the buffer visually)
  (setq org-hide-emphasis-markers t)
  ;; Show "real" bullet point markers (only works for bullet points with dash (-))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (font-lock-add-keywords 'org-mode
                          '(("^ *-.*\\( ::\\) "
                             (0 (prog1 () (compose-region (match-beginning 1)
							  (match-end 1) "")))))))
(defun my-org/post-init-org ()
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  ;; Load everything after org
  (with-eval-after-load 'org
  ;;; enable babel languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (gnuplot . t)
       (ruby . t)
       (ditaa . t)
       (plantuml . t)
       (sh . t)
       (shell . t)))
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
  (with-eval-after-load 'ox-latex
    ;; Use xelatex for unicode support
    (setq org-latex-pdf-process
	  '("xelatex -interaction nonstopmode %f"
	    "xelatex -interaction nonstopmode %f"))
    (setq org-export-latex-listings 'minted)
    ;;(add-to-list 'org-export-latex-packages-alist '("" "minted"))
    ;; Add the org-mode latex class that I use
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))
    (add-to-list 'org-latex-classes
		 '("default"
		   "\\documentclass[11pt]{article}\n
\\usepackage[a4paper, margin=2.5cm]{geometry}"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsecton*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  ;; Exit snippet editing with ~, ,~
  (with-eval-after-load 'org-src
    ;; When editing a code snippet, use the current window rather than popping
    ;; open a new one (which shows the same information)
    (setq org-src-window-setup 'current-window)
    (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
      "," 'org-edit-src-exit)))

;;; packages.el ends here
