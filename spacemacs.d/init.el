					;~/.emacs.d/elpa/helm-gitignore*; -*- mode: emacs-lisp -*-
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
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     (version-control :variables
		      version-control-diff-tool 'diff-hl
		      version-control-global-margin t)
     spacemacs-layer-extracts
     themes-megapack
     rcirc
     shell-scripts
     games
     python
     (auto-completion
      :disabled-for org)

     (c-c++ :variables c-c++-enable-clang-support t
	    c-c++-default-mode-for-headers 'c++-mode)
     emacs-lisp
     git
     latex
     ruby
     go
     ranger
     html
     javascript
     (shell :variables
	    shell-default-position 'full
	    shell-default-term-shell "/bin/zsh"
	    shell-default-shell 'multi-term)
     markdown
     org
     eyebrowse
     syntax-checking
     (spell-checking :variables spell-checking-enable-by-default nil)
     spacemacs-layouts
     evil-snipe
     vim-empty-lines
     dockerfile
     ansible
     emms
     emoji
     erc
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(editorconfig yaml-mode pdf-tools esup centered-window-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
			 solarized-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
			       :size 9
			       :weight normal
			       :width normal
			       :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize 1
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
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
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-line-numbers nil
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (setq-default ranger-override-dired t)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;;;;Terminal
  (evil-leader/set-key "ote" 'eshell)
  (evil-leader/set-key "ott" 'multi-term)
  (evil-leader/set-key "otn" 'multi-term-next)
  (evil-leader/set-key "otp" 'multi-term-prev)
  (evil-leader/set-key "otl" 'multi-term-next)
  (evil-leader/set-key "oth" 'multi-term-prev)
  (evil-leader/set-key "o <SPC>" 'helm-multi-files)
  (evil-leader/set-key "ot <SPC>" 'multi-term-dedicated-toggle)

  (evil-leader/set-key "oo" 'spacemacs/workspaces-micro-state)
  (evil-leader/set-key
    "pO" 'helm-projectile-find-file-dwim
    "cf" 'make-flash)
  (defun make-flash () (interactive) (compile "make flash"))
  (setq tramp-default-method "ssh")
  (setq tab-width 8)
  (fringe-mode 4)
  ;; I'm not scared of saving everything.
  (setq compilation-ask-about-save nil)
  ;; Stop on the first error.
  (setq compilation-scroll-output 'next-error)
  ;; Don't stop on info or warnings.
  (setq compilation-skip-threshold 2)
  (setq dired-listing-switches "-alh")
  (setq org-agenda-files (list "~/Documents/org/private/programming.org"))
  (setq neo-theme 'ascii)
  (setq powerline-default-separator 'nil)
  (spacemacs/toggle-centered-point-globally-on)
  (setq diff-hl-side 'left)
  ;;Disable centered cursor mode for shell-like modes
  (define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
    (lambda ()
      (when (not (memq major-mode
		       (list 'slime-repl-mode 'shell-mode 'term-mode
			     'eshell-mode 'rcirc-mode 'erc-mode)))
	(centered-cursor-mode))))
  (my-global-centered-cursor-mode 1)

  ;;Disable evil for teminal modes
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq-default tab-width 8)
  (setq-default indent-tabs-mode t)
  (setq c-default-style "linux")
  (spacemacs/toggle-auto-fill-mode-on)
  (spacemacs/toggle-truncate-lines-off)
  (use-package editorconfig
    :config (editorconfig-mode 1))
  (pdf-tools-install)
  ;; Org mode html stylesheet
  ;; Source: https://stackoverflow.com/questions/19614104/how-to-tell-org-mode-to-embed-my-css-file-on-html-export
  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
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
  ;; Incease gcons threshhold to reduce freezinc
  (setq gc-cons-threshold '20000000)
  (defun nothing())
  ;; Disable the mouse
  (define-key evil-normal-state-map (kbd "<down-mouse-1>") 'nothing)
  (dolist (mouse '("<down-mouse-1>" "<mouse-1>"))
    (global-unset-key (kbd mouse))) (centered-window-mode t)
    )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 )
