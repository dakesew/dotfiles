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
     ruby
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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
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
  ;; Disable colorscheme in terminal
  (add-to-list 'default-frame-alist '(tty-color-mode . -1))
  ;; Use ssh as the default connect method in tramp
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; Set calc-config file in .spacemacs.d
  (setq calc-settings-file "~/.spacemacs.d/calc.el")
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

  ;; WARNING SECURITY FIX!!! http://seclists.org/oss-sec/2017/q3/422
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28350
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(dired-recursive-deletes (quote always))
 '(dired-recursive-copies (quote always))
 '(evil-want-Y-yank-to-eol nil)
 '(eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q=")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-credential-cache-daemon-socket "/home/soryio/.git-credential-cache/socket")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-babel-load-languages (quote ((python . t) (gnuplot . t) (ditaa . t))))
 '(org-calc-default-modes
   (quote
    (calc-internal-prec 12 calc-float-format
			(eng 5)
			calc-angle-mode deg calc-prefer-frac nil calc-symbolic-mode nil calc-date-format
			(YYYY "-" MM "-" DD " " Www
			      (" " hh ":" mm))
			calc-display-working-message t)))
 '(org-ditaa-eps-jar-path "/usr/share/java/ditaa-eps/DitaaEps.jar")
 '(org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
 '(package-selected-packages
   (quote
    (powershell graphviz-dot-mode lacarte clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode toml-mode racer flycheck-rust seq cargo rust-mode fuzzy ascii plantuml-mode babel nasm-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy mu4e-maildirs-extension mu4e-alert company-emacs-eclim eclim autothemer wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode org-page git org mustache ht phpunit phpcbf php-extras nginx-mode hackernews php-auto-yasnippets drupal-mode php-mode forth-mode intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode vimrc-mode dactyl-mode ttrss libmpdee mingus go-guru epresent darkroom eimp pug-mode minitest insert-shebang hide-comnt ox-gfm rainbow-mode docker docker-tramp window-numbering volatile-highlights uuidgen paradox spinner neotree move-text linum-relative link-hint info+ indent-guide hungry-delete highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-mode-manager helm-make helm-ag google-translate golden-ratio flyspell-correct-helm flx-ido fancy-battery expand-region evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-link ace-jump-helm-line yapfify typit mmt rake py-isort org-projectile org-download livid-mode skewer-mode simple-httpd live-py-mode jinja2-mode hydra git-link flyspell-correct-popup flyspell-correct goto-chg undo-tree eshell-z diminish darkokai-theme company-shell ace-window zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler web-mode web-beautify underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline powerline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode pcre2el rubocop rspec-mode robe reverse-theme restart-emacs rcirc-notify rcirc-color rbenv ranger rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-yapf purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pdf-tools tablist pastels-on-dark-theme page-break-lines pacmacs orgit organic-green-theme org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow macrostep lush-theme lua-mode lorem-ipsum light-soap-theme less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme ido-vertical-mode hy-mode htmlize hl-todo heroku-theme hemisu-theme help-fns+ helm-pydoc helm-projectile projectile helm-gitignore request helm-flyspell helm-flx flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md gandalf-theme flycheck-pos-tip flycheck pkg-info epl flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme eyebrowse exec-path-from-shell evil-visualstar evil-surround evil-snipe evil-numbers evil-magit magit magit-popup git-commit with-editor evil-escape esup espresso-theme eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus helm helm-core async emms emmet-mode elisp-slime-nav editorconfig dracula-theme dockerfile-mode django-theme disaster diff-hl darktooth-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-go go-mode company-emoji company-c-headers company-auctex company-anaconda company colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clang-format chruby cherry-blossom-theme centered-window-mode busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme avy auto-yasnippet yasnippet auto-dictionary auto-compile packed auctex-latexmk auctex apropospriate-theme anti-zenburn-theme ansible-doc ansible anaconda-mode pythonic f s ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete popup 2048-game quelpa package-build use-package which-key bind-key bind-map evil solarized-theme dash)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(safe-local-variable-values (quote ((org-export-allow-bind-keywords . true))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tls-checktrust (quote ask))
 '(tramp-default-method "ssh")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yaml-indent-offset 8))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
