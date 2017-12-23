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
 '(dired-recursive-copies (quote always))
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
    (evil-collection powershell graphviz-dot-mode lacarte clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode toml-mode racer flycheck-rust seq cargo rust-mode fuzzy ascii plantuml-mode babel nasm-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy mu4e-maildirs-extension mu4e-alert company-emacs-eclim eclim autothemer wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode org-page git org mustache ht phpunit phpcbf php-extras nginx-mode hackernews php-auto-yasnippets drupal-mode php-mode forth-mode intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode vimrc-mode dactyl-mode ttrss libmpdee mingus go-guru epresent darkroom eimp pug-mode minitest insert-shebang hide-comnt ox-gfm rainbow-mode docker docker-tramp window-numbering volatile-highlights uuidgen paradox spinner neotree move-text linum-relative link-hint info+ indent-guide hungry-delete highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-mode-manager helm-make helm-ag google-translate golden-ratio flyspell-correct-helm flx-ido fancy-battery expand-region evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-ediff evil-args evil-anzu anzu eval-sexp-fu highlight dumb-jump define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol aggressive-indent adaptive-wrap ace-link ace-jump-helm-line yapfify typit mmt rake py-isort org-projectile org-download livid-mode skewer-mode simple-httpd live-py-mode jinja2-mode hydra git-link flyspell-correct-popup flyspell-correct goto-chg undo-tree eshell-z diminish darkokai-theme company-shell ace-window zonokai-theme zenburn-theme zen-and-art-theme yaml-mode xterm-color ws-butler web-mode web-beautify underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline powerline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode pcre2el rubocop rspec-mode robe reverse-theme restart-emacs rcirc-notify rcirc-color rbenv ranger rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-yapf purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pdf-tools tablist pastels-on-dark-theme page-break-lines pacmacs orgit organic-green-theme org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow macrostep lush-theme lua-mode lorem-ipsum light-soap-theme less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme ido-vertical-mode hy-mode htmlize hl-todo heroku-theme hemisu-theme help-fns+ helm-pydoc helm-projectile projectile helm-gitignore request helm-flyspell helm-flx flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md gandalf-theme flycheck-pos-tip flycheck pkg-info epl flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme eyebrowse exec-path-from-shell evil-visualstar evil-surround evil-snipe evil-numbers evil-magit magit magit-popup git-commit with-editor evil-escape esup espresso-theme eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus helm helm-core async emms emmet-mode elisp-slime-nav editorconfig dracula-theme dockerfile-mode django-theme disaster diff-hl darktooth-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-go go-mode company-emoji company-c-headers company-auctex company-anaconda company colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clang-format chruby cherry-blossom-theme centered-window-mode busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme avy auto-yasnippet yasnippet auto-dictionary auto-compile packed auctex-latexmk auctex apropospriate-theme anti-zenburn-theme ansible-doc ansible anaconda-mode pythonic f s ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete popup 2048-game quelpa package-build use-package which-key bind-key bind-map evil solarized-theme dash)))
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
