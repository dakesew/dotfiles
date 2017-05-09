;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     spacemacs-evil
     ivy
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
     python
     shell-scripts
     syntax-checking
     vim-empty-lines
     spacemacs-completion
     rust
     clojure)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(editorconfig mingus rainbow-mode babel color-theme-sanityinc-solarized material-theme)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Add a custom title for spacemacs buffer
   spacemacs-buffer-logo-title "[E M A C S]"
   ;; Add a custom startup banner
   ;; dotspacemacs-startup-banner (concat d12-path/emacs-private "animacs-banner.png")
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '(agenda
                                todos
                                (recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         solarized-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 9
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state 1
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
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
							  (match-end 1) ""))))))
  (add-to-list 'load-path "~/.spacemacs.d/")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;; Why isn't this here already?
  (spacemacs/set-leader-keys "gc" 'magit-clone)
  ;; Make a key binding for avy
  (spacemacs/set-leader-keys "j <SPC>" 'avy-goto-char-timer)
  (spacemacs/set-leader-keys "jl" 'avy-goto-line)
  ;; use eshell instead of shell command
  (spacemacs/set-leader-keys "!" 'eshell-command)
  ;; Setup esh eldoc
  (setup-esh-help-eldoc)
  ;; Use ivy for eshell completion https://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completionhttps://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completion
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (define-key eshell-mode-map (kbd "C-r")
		(lambda () (interactive) (counsel-esh-history)))))
  ;; Define permanent eshell aliases
  (eshell/alias "cover" "wget -O cover.jpg {xclip -o}")
  (eshell/alias "gain" "mp3gain -r $* && mp3gain -s d $*")
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
  (setq compilation-ask-about-save nil)
  ;; Stop on the first error.
  (setq compilation-scroll-output 'next-error)
  ;; Don't stop on info or warnings.
  (setq compilation-skip-threshold 2)
  (setq dired-listing-switches "-lAFaGh1v --si --time-style long-iso --group-directories-first")
  (setq powerline-default-separator 'nil)
  (setq diff-hl-side 'left)
  ;;Disable evil for teminal modes
  (evil-set-initial-state 'term-mode 'emacs)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq-default tab-width 8)
  (setq-default indent-tabs-mode t)
  (setq c-default-style "linux")
  (spacemacs/toggle-truncate-lines-off)
  (use-package editorconfig
    :config (editorconfig-mode 1))
  ;; Org mode html stylesheet
  ;; Source: https://stackoverflow.com/questions/19614104/how-to-tell-org-mode-to-embed-my-css-file-on-html-export
  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (or buffer-file-name "DEFAULT-NAME"))))
             (path (concat dir "style.css"))
             (homestyle (or (null dir) (null (file-exists-p path))))
             (final (if homestyle "~/.spacemacs.d/org-style.css" path))) ;; <- set your own style file path
        (setq org-html-head-include-default-style nil)
        (setq org-html-head (concat
                             "<style type=\"text/css\">\n"
                             "<!--/*--><![CDATA[/*><!--*/\n"
                             (with-temp-buffer
                               (insert-file-contents final)
                               (buffer-string))
                             "/*]]>*/-->\n"
                             "</style>\n")))))
  (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)
  ;; Load everything after org
  (with-eval-after-load 'org
    ;; Set inline image size
    (setq org-image-actual-width (/ (display-pixel-width) 4))
    ;; Define a custom link type to reference dead paper
    (org-add-link-type "paper" 'org-paper-open)
    (defun org-paper-open (path)
      (message path)))
  (with-eval-after-load 'ox-latex
    (setq org-export-latex-listings 'minted)
    ;(add-to-list 'org-export-latex-packages-alist '("" "minted"))
    ;; Add the org-mode latex class that I use
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))
    (add-to-list 'org-latex-classes
                 '("default"
                   "\\documentclass[11pt]{article}\n
\\usepackage[a4paper, margin=2.5cm]{geometry}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  ;; Exit snippet editing with ~, ,~
  (with-eval-after-load 'org-src
    ;; When editing a code snippet, use the current window rather than popping
    ;; open a new one (which shows the same information)
    (setq org-src-window-setup 'current-window)
    (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
      "," 'org-edit-src-exit))
  ;; Incease gcons threshhold to reduce freezing
                                        ;(setq gc-cons-threshold '20000000)
  (defun nothing() (interactive))
  ;; Disable the mouse
  (define-key evil-normal-state-map (kbd "<down-mouse-1>") 'nothing)
  (define-key evil-normal-state-map (kbd "<mouse-1>") 'nothing)
  ;; Disable colorscheme in terminal
  (add-to-list 'default-frame-alist '(tty-color-mode . -1))
  ;; Use ssh as the default connect method in tramp
  (setq tramp-default-method "ssh")
  ;; Set calc-config file in .spacemacs.d
  (setq calc-settings-file "~/.spacemacs.d/calc.el")
  (setq calc-multiplication-has-precedence nil)
  (load calc-settings-file t)
  ;; enable babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (gnuplot . t)
     (ditaa . t)
     (sh . t)
     (shell . t)))
  ;; gas mode
  (require 'gas-mode)
  ;; ttrss
  (require 'ttrss)
  (require 'gas-mode)
  (add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
  (spacemacs/toggle-auto-fill-mode-on)
  ;; So tramp remebers passwords https://stackoverflow.com/questions/840279/passwords-in-emacs-tramp-mode-editing
  (setq password-cache-expiry nil)
  ;; Don't show the output buffer of async shell commands 
  (add-to-list 'display-buffer-alist (cons "\\Async Shell Command\\.*" (cons
									#'display-buffer-no-window
									nil)))
  ;; Add directories to recentf too
  ;; https://www.emacswiki.org/emacs/recentf-ext.el
  (defun recentf-add-dired-directory ()
    (when (and (stringp dired-directory)
	       (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)

  (dired-async-mode t)
  (setq calc-show-banner nil)
  (setq calc-full-mode t)
  (setq calc-window-height 34)
  (setq calc-language 'big)
  (setq calc-display-strings t)
  (setq calc-full-float-format '(eng 0))
  (setq calc-float-format '(eng 6))
  (setq calc-group-digits t))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
 '(org-agenda-files (quote ("~/usr/log/agenda.org")))
 '(org-babel-load-languages (quote ((python . t) (gnuplot . t) (ditaa . t))))
 '(package-selected-packages
   (quote
    (lacarte clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode toml-mode racer flycheck-rust seq cargo rust-mode fuzzy ascii plantuml-mode babel nasm-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy mu4e-maildirs-extension mu4e-alert company-emacs-eclim eclim autothemer wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode org-page git org mustache ht phpunit phpcbf php-extras nginx-mode hackernews php-auto-yasnippets drupal-mode php-mode forth-mode intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode vimrc-mode dactyl-mode ttrss libmpdee mingus go-guru epresent darkroom eimp pug-mode minitest insert-shebang hide-comnt ox-gfm rainbow-mode docker docker-tramp window-numbering volatile-highlights uuidgen paradox spinner neotree move-text linum-relative link-hint info+ indent-guide hungry-delete highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-mode-manager helm-make helm-ag google-translate golden-ratio flyspell-correct-helm flx-ido fancy-battery expand-region evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-link ace-jump-helm-line yapfify typit mmt rake py-isort org-projectile org-download livid-mode skewer-mode simple-httpd live-py-mode jinja2-mode hydra git-link flyspell-correct-popup flyspell-correct goto-chg undo-tree eshell-z diminish darkokai-theme company-shell ace-window zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler web-mode web-beautify underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline powerline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode pcre2el rubocop rspec-mode robe reverse-theme restart-emacs rcirc-notify rcirc-color rbenv ranger rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-yapf purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pdf-tools tablist pastels-on-dark-theme page-break-lines pacmacs orgit organic-green-theme org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow macrostep lush-theme lua-mode lorem-ipsum light-soap-theme less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme ido-vertical-mode hy-mode htmlize hl-todo heroku-theme hemisu-theme help-fns+ helm-pydoc helm-projectile projectile helm-gitignore request helm-flyspell helm-flx flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md gandalf-theme flycheck-pos-tip flycheck pkg-info epl flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme eyebrowse exec-path-from-shell evil-visualstar evil-surround evil-snipe evil-numbers evil-magit magit magit-popup git-commit with-editor evil-escape esup espresso-theme eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus helm helm-core async emms emmet-mode elisp-slime-nav editorconfig dracula-theme dockerfile-mode django-theme disaster diff-hl darktooth-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-go go-mode company-emoji company-c-headers company-auctex company-anaconda company colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clang-format chruby cherry-blossom-theme centered-window-mode busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme avy auto-yasnippet yasnippet auto-dictionary auto-compile packed auctex-latexmk auctex apropospriate-theme anti-zenburn-theme ansible-doc ansible anaconda-mode pythonic f s ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete popup 2048-game quelpa package-build use-package which-key bind-key bind-map evil solarized-theme dash)))
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
