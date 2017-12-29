;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     sql
     extra-langs
     nginx
     csv
     yaml
     asciidoc
     (ruby :variables ruby-enable-enh-ruby-mode t)
     python
     windows-scripts
     markdown
     spacemacs-evil
     ivy
     lua
     (auto-completion :disabled-for
                      org
                      eshell)
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode)
     (org :variables
          org-enable-github-support t
          org-startup-indented t)
     spacemacs-org
     (shell :variables
            shell-default-term-shell "/bin/zsh"
            shell-default-position 'full
            shell-default-shell 'eshell)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     docker
     emacs-lisp
     git
     html
     javascript
     latex
     shell-scripts
     syntax-checking
     spacemacs-completion
     spacemacs-ui-visual
     rust
     graphviz
     plantuml
     pdf-tools
     (rcirc :variables rcirc-enable-authinfo-support t)
     my-dired
     my-org
     clojure)
   dotspacemacs-additional-packages '(editorconfig mingus rainbow-mode
  color-theme-sanityinc-solarized material-theme rhtml-mode evil-collection
  avandu kotlin-mode )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(exec-path-from-shell)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   spacemacs-buffer-logo-title "Emacs"
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-dark
                         material-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 11
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-micro-state 1
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq custom-file "~/.spacemacs.d/elisp/custom-settings.el")
  (add-to-list 'load-path "~/.spacemacs.d/")
  (defun exec-path-from-shell-copy-env (name) ())
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (setq auth-sources
	'((:source "~/.spacemacs.d/authinfo.gpg")))
  ;; Why isn't this here already?
  (spacemacs/set-leader-keys "gc" 'magit-clone)
  ;; Make a key binding for avy
  (spacemacs/set-leader-keys "j <SPC>" 'avy-goto-char-timer)
  (spacemacs/set-leader-keys "jl" 'avy-goto-line)
  ;; use eshell instead of shell command
  (spacemacs/set-leader-keys "!" 'eshell-command)
  (require 'em-tramp)
  (require 'em-cmpl)
  (require 'em-xtra)
  ;; Add reverse enter
  (defun reverse-newline ()
    (interactive)
    (beginning-of-line)
    (newline)
    (previous-line))
  (global-set-key [(control return)] 'reverse-newline)
  ;; Setup esh eldoc
  (setup-esh-help-eldoc)
  (setq ivy-use-selectable-prompt t)
  ;; Use ivy for eshell completion https://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completionhttps://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completion
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (define-key eshell-mode-map (kbd "C-r")
          (lambda () (interactive) (counsel-esh-history)))))
  (with-eval-after-load 'em-alias
    ;; Define permanent eshell aliases
    ;; Somehow system sudo is called per default, and it has strange behaviour
    (eshell/alias "sudo" "eshell/sudo $*")
    (eshell/alias "ff" "find-file")
    (eshell/alias "ee" "find-file-other-window")
    (eshell/alias "cover" "wget -O cover.jpg {xclip -o}")
    (eshell/alias "gain" "mp3gain -r ./**/*.mp3"))
  (defun eshell/d ()
    (dired "."))
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
  (spacemacs/set-leader-keys "'" 'eshell-open)
  (spacemacs/set-leader-keys "." 'eshell)
  ;; Shared history.
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

  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (eshell-hist-use-global-history)
	      (local-set-key (kbd "M-P") 'eshell-previous-prompt)
	      (local-set-key (kbd "M-N") 'eshell-next-prompt)
	      (local-set-key (kbd "M-R") 'eshell-list-history)
	      (local-set-key (kbd "M-r")
			     (lambda ()
			       (interactive)
			       (insert
				(ido-completing-read "Eshell history: "
						     (delete-dups
						      (ring-elements eshell-history-ring))))))))

  ;; Locate is faster when not ignoring case and we usually don't need more than
  ;; the first few lines of output 
  (defun counsel-locate-cmd-default (input)
    "Return a shell command based on INPUT."
    (counsel-require-program "locate")
    (format "locate -n 100 --regex '%s'"
	    (counsel-unquote-regex-parens
	     (ivy--regex input))))

  ;; Easily edit files as root
  (defun user/edit-as-root ()
    "Open the current file as root"
    (interactive)
    (let ((file (buffer-file-name)))
      (unless (file-writable-p file)
        (setq file (concat "/sudo:root@localhost:" file)))
      (find-file file)))
  (spacemacs/set-leader-keys "os" 'user/edit-as-root)

  (spacemacs/set-leader-keys
    "cf" 'make-flash)
  (defun make-flash () (interactive) (compile "make flash"))
  (fringe-mode 4)
  ;; I'm not scared of saving everything.
  (setq compilation-ask-about-save nil) ;; Stop on the first error.
  (setq compilation-scroll-output 'next-error)
  ;; Don't stop on info or warnings.
  (setq compilation-skip-threshold 2)
  (setq powerline-default-separator 'nil)
  (setq diff-hl-side 'left)
  ;;Disable evil for teminal modes
  (evil-set-initial-state 'term-mode 'emacs)

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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))

  (setq-default tab-width 8
                indent-tabs-mode t
		standard-indent 8)
  (setq c-default-style "linux")
  (spacemacs/toggle-truncate-lines-off)
  (use-package editorconfig
    :config (editorconfig-mode 1))
  ;; Incease gcons threshhold to reduce freezing
  ;;(setq gc-cons-threshold '20000000)
  (defun nothing() (interactive))
  ;; Disable the mouse
  (define-key evil-normal-state-map (kbd "<down-mouse-1>") 'nothing)
  (define-key evil-normal-state-map (kbd "<mouse-1>") 'nothing)
  (define-key evil-normal-state-map (kbd "<mouse-3>") 'nothing)
  (define-key evil-normal-state-map (kbd "<mouse-2>") 'nothing)
  (define-key evil-normal-state-map (kbd "<drag-mouse-1>") 'nothing)
  ;; Disable colorscheme in terminal
  (add-to-list 'default-frame-alist '(tty-color-mode . -1))
  ;; Use ssh as the default connect method in tramp
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; Set calc-config file in .spacemacs.d
  (setq calc-settings-file "~/.spacemacs.d/custom-calc.el")
  (setq calc-multiplication-has-precedence nil)
  (load calc-settings-file t)
  ;; gas mode
  (require 'gas-mode)
  ;; tt-rss
  (require 'avandu)
  (setq avandu-tt-rss-api-url "https://dsawatzke.duckdns.org/tt-rss/api/")
  (spacemacs/set-leader-keys-for-major-mode 'avandu-article-mode "q"
    (lambda () (interactive) (switch-to-buffer "*avandu-overview*")))
  ;; Constants
  (require 'constants)
  (add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))
  (spacemacs/toggle-auto-fill-mode-on)
  ;; So tramp remebers passwords https://stackoverflow.com/questions/840279/passwords-in-emacs-tramp-mode-editing
  (setq password-cache-expiry nil)
  ;; Don't show the output buffer of async shell commands
  (add-to-list 'display-buffer-alist (cons "\\Async Shell Command\\.*" (cons
									#'display-buffer-no-window
									nil)))
  (defun copy-name ()
    (interactive)
    (let ((file-name (or (buffer-file-name) list-buffers-directory)))
      (if file-name
	  (message (kill-new (file-name-nondirectory file-name)))
	(error "Buffer not visiting a file"))))
  (setq calc-show-banner nil)
  (setq calc-full-mode t)
  (setq calc-window-height 34)
  (setq calc-language 'big)
  (setq calc-display-strings t)
  (setq calc-full-float-format '(eng 0))
  (setq calc-float-format '(eng 6))
  (setq calc-group-digits t)
  (defun my-compile ()
    (interactive)
    (let ((default-directory (locate-dominating-file "." "Makefile")))
      (compile "make")))
  (setq lua-documentation-function 'eww)
  (setq evil-want-Y-yank-to-eol t)
  ;; Shift by tab
  (setq-default evil-shift-width 8)
  (setq neo-theme 'ascii)
  ;; Fold ruby(eval-after-load "hideshow"
  (eval-after-load "hideshow"
    '(add-to-list 'hs-special-modes-alist
		  `(ruby-mode
		    ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
		    ,(rx (or "}" "]" "end"))                       ; Block end
		    ,(rx (or "#" "=begin"))                        ; Comment start
		    ruby-forward-sexp nil)))
  ;; Enable evil-collection (more vi-like keybindings)
  (require 'evil-collection-minibuffer)
  (evil-collection-minibuffer-setup)
  (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  (with-eval-after-load 'calendar
    (require 'evil-collection-calendar)
    (evil-collection-calendar-setup))
  (with-eval-after-load 'compile
    (require 'evil-collection-compile)
    (evil-collection-compile-setup))
  (with-eval-after-load 'eww
    (require 'evil-collection-eww)
    (evil-collection-eww-setup))
  (with-eval-after-load 'ibuffer
    (require 'evil-collection-ibuffer)
    (evil-collection-ibuffer-setup))
  (with-eval-after-load 'proced
    (require 'evil-collection-proced)
    (evil-collection-proced-setup))
  (defvar evil-collection-setup-minibuffer t)
  (with-eval-after-load 'ivy
    (require 'evil-collection-ivy)
    (evil-collection-ivy-setup))
  ;; WARNING SECURITY FIX!!! http://seclists.org/oss-sec/2017/q3/422
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28350
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))
