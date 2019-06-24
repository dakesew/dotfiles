;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


(load! "+bindings")
(load! "+dired")
(load! "xournalpp")

(setq evil-escape-key-sequence "nr")

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
                                        ;(remove-hook 'doom-post-init-hook #'shackle-mode)
                                        ;(remove-hook 'doom-popup-mode-hook #'doom|hide-modeline-in-popup)
(remove-hook 'after-change-major-mode-hook #'doom|highlight-non-default-indentation)

(defun evil-cursor-proper (&rest _)
  (setq evil-default-cursor '(t "#f0c674")
        evil-visual-state-cursor '("#81a2be" hollow)
        evil-emacs-state-cursor '("#5a5b5a" box)
        evil-replace-state-cursor '("#de935f" box)
        evil-normal-state-cursor '("#f0c674" box)
        evil-motion-state-cursor '("#b294bb" box)
        evil-insert-state-cursor '("#b5bd68" bar)))
(advice-add #'load-theme :after #'evil-cursor-proper)

;; Locate is faster when not ignoring case and we usually don't need more than
;; the first few lines of output
(defun counsel-locate-cmd-default (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "locate")
  (format "locate -n 100 --regex '%s'"
          (counsel-unquote-regex-parens
           (ivy--regex input))))

;; https://scripter.co/narrowing-the-author-column-in-magit/
(defun magit-log--abbreviate-author (&rest args)
  "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names."
  ;; ARGS             -> '((AUTHOR DATE))
  ;; (car ARGS)       -> '(AUTHOR DATE)
  ;; (car (car ARGS)) -> AUTHOR
  (let* ((author (car (car args)))
         (author-abbr (if (string-match-p "," author)
                          ;; Last, First -> F Last
                          (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                        ;; First Last -> F Last
                        (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
    (setf (car (car args)) author-abbr))
  (car args))                       ;'(AUTHOR-ABBR DATE)
(advice-add 'magit-log-format-margin :filter-args #'magit-log--abbreviate-author)
(setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))

;; So tramp remebers passwords https://stackoverflow.com/questions/840279/passwords-in-emacs-tramp-mode-editing
(setq password-cache-expiry nil)
;; Don't show the output buffer of async shell commands
(add-to-list 'display-buffer-alist (cons "\\Async Shell Command\\.*" (cons
                                                                      #'display-buffer-no-window
                                                                      nil)))
(setq calc-show-banner nil)
(setq calc-full-mode t)
(setq calc-window-height 34)
(setq calc-language 'big)
(setq calc-display-strings t)
(setq calc-full-float-format '(eng 0))
(setq calc-float-format '(eng 6))
(setq calc-group-digits t)
(setq math-additional-units '(
                              (bits nil "bits")
                              (bytes "8 * bits" "bytes")

                              (tib "1000 * gib"    "TiB")
                              (gib "1000 * mib"    "GiB")
                              (mib "1000 * kib"    "MiB")
                              (kib "1000 * bytes"  "KiB")

                              (tb  "1024 * gb"      "TB")
                              (gb  "1024 * mb"      "GB")
                              (mb  "1024 * kb"      "MB")
                              (kb  "1024 * bytes"   "KB")
                              ))

;; Reduce scroll lag
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

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

;; Neo specific changes
(setq-default avy-keys '(?u ?i ?a ?e ?n ?r ?t ?d))
(setq avy-keys '(?u ?i ?a ?e ?n ?r ?t ?d))

;; TODO: This is to slow
(defvar doom-modeline-evil-state-color
  '((normal . "#f0c674")
    (insert . "#b5bd68")
    (emacs . "#5a5b5a")
    (replace . "#de935f")
    (visual . "#81a2be")
    (motion . "#b294bb")))

(defun doom-modeline-highlight-face-evil-state ()
  "Set the highlight face depending on the evil state.
Set `doom-modeline-highlight-face-func' to
`doom-modeline-highlight-face-evil-state' to use this."
  (if (bound-and-true-p evil-local-mode)
      (let* ((color (cdr (assq evil-state doom-modeline-evil-state-color))))
        (set-face-attribute 'doom-modeline-bar nil :background color)
        (set-face-attribute 'doom-modeline-panel t :background color))))

(advice-add 'doom-modeline-format--main :before  #'doom-modeline-highlight-face-evil-state)
(advice-remove 'doom-modeline-format--main  #'doom-modeline-highlight-face-evil-state)

(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))
(after! ivy
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  )

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
        ("gain" "mp3gain -r ./**/*.mp3")))

(setq
 +doom-modeline-height 15
 ivy-posframe-font (font-spec :family "Source Code Pro" :size 10)
 doom-font (font-spec :family "Source Code Pro" :size 10))


(setq
 indend-tabs-mode t)

;; For Org preview
(require 'ox-latex)
(setq org-latex-packages-alist 'nil)
(setq org-latex-default-packages-alist
  '(("AUTO" "inputenc"  t ("pdflatex"))
    ("T1"   "fontenc"   t ("pdflatex"))
    (""     "graphicx"  t)
    (""     "grffile"   t)
    (""     "longtable" nil)
    (""     "wrapfig"   nil)
    (""     "rotating"  nil)
    ("normalem" "ulem"  t)
    (""     "amsmath"   t)
    (""     "amssymb"   t)
    (""     "unicode-math"   t)
    (""     "textcomp"  t)
    (""     "capt-of"   nil)
    (""     "hyperref"  nil)))
(add-to-list 'org-preview-latex-process-alist '(imagexetex :programs
         ("xetex" "convert")
         :description "pdf > png" :message "you need to install the programs: xetex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
         (1.0 . 1.0)
         :latex-compiler
         ("xelatex -interaction nonstopmode -output-directory %o %f")
         :image-converter
         ("convert -density %D -trim -antialias %f -quality 100 %O")))
;; From https://github.com/LionyxML/ros
;; Rahul Org-mode Screenshot takes a screenshot with scrot -s (waits for screen
;; selection), saves it as orgfileopened.org_YYYYMMDD_hhmmss.png, inserts
;; the link and turns on the display-inline-images, showing your screenshot directly
;; to the org-file"
(add-hook 'org-mode-hook
	  (lambda ()
	  (defun ros ()
	  (interactive)
	  (let ((filename
		 (concat "./"
			 (file-name-nondirectory buffer-file-name)
			 "_"
			 (format-time-string "%Y%m%d_%H%M%S")
			 ".png")))
	    (call-process "scrot" nil nil nil "-s" filename)
	    (insert (concat "[[" filename "]]"))
	    (org-display-inline-images t t)))))

(turn-on-org-cdlatex)
(setq org-preview-latex-default-process 'imagexetex
      org-catch-invisible-edits 'show-and-error
      c-default-style "linux"
      indent-tabs-mode t)
(after!  smartparens
  (smartparens-global-mode -1))
(setq cdlatex-command-alist '(("tt" "insert text element" "\\text{?}" cdlatex-position-cursor nil nil t)))

(mouse-wheel-mode -1)

(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)

(autoload 'ojs-export-to-blog "ox-jekyll-subtree")
(setq org-jekyll-use-src-plugin t)

(setq ojs-blog-base-url "https://blowenfusen.dev/")
(setq ojs-blog-dir (expand-file-name "~/usr/blog/jekyll/"))
(setq select-enable-primary t)
