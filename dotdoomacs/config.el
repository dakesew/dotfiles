;;; private/default/config.el -*- lexical-binding: t; -*-

(load! +bindings)
(load! +myorg)
(load! +mydired)
(load! +myeshell)

;;
;; Plugins
;;

(def-package! emacs-snippets :after yasnippet)
(def-package! evil-magit :after magit :config (define-key magit-mode-map " " nil))

;;
;; Config
;;
(setq yas-user-snippets-dir (expand-file-name "modules/private/default/snippets/" user-emacs-directory))
(after! yasnippet
  (setq yas-snippet-dirs (append (list 'yas-user-snippets-dir ) (yas-snippet-dirs))))
(setq yas-snippet-dirs '())
(setq doom-font (font-spec :family "Source Code Pro" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro")
      doom-unicode-font (font-spec :family "Source Code Pro" :size 12)
      doom-big-font (font-spec :family "Source Code Pro" :size 20))
(setq ivy-use-selectable-prompt t)
(setq evil-escape-key-sequence "nr")
(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))


(when (featurep 'evil)
  (load! +evil-commands)

  ;; Makes ; and , the universal repeat-keys in evil-mode
  (defmacro do-repeat! (command next-func prev-func)
    "Repeat motions with ;/,"
    (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
      `(progn
         (defun ,fn-sym (&rest _)
           (define-key evil-motion-state-map (kbd ";") ',next-func)
           (define-key evil-motion-state-map (kbd ",") ',prev-func))
         (advice-add #',command :before #',fn-sym))))

  ;;; n/N
  ;(do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  ;(do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  ;(do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  ;(do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

  ;; f/F/t/T/s/S
  (after! evil-snipe
    (setq evil-snipe-repeat-keys nil
          evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;

    (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

  ;; */#
  (after! evil-visualstar
    (do-repeat! evil-visualstar/begin-search-forward
                evil-ex-search-next evil-ex-search-previous)
    (do-repeat! evil-visualstar/begin-search-backward
                evil-ex-search-previous evil-ex-search-next))

  ;; lazy-load `evil-easymotion'
  (map! :m "gs" #'+default/easymotion)
  (defun +default/easymotion ()
    (interactive)
    (let ((prefix (this-command-keys)))
      (evilem-default-keybindings prefix)
      (map! :map evilem-map
            "n" (evilem-create #'evil-ex-search-next)
            "N" (evilem-create #'evil-ex-search-previous)
            "s" (evilem-create #'evil-snipe-repeat
                               :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                               :bind ((evil-snipe-scope 'buffer)
                                      (evil-snipe-enable-highlight)
                                      (evil-snipe-enable-incremental-highlight)))
            "S" (evilem-create #'evil-snipe-repeat-reverse
                               :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                               :bind ((evil-snipe-scope 'buffer)
                                      (evil-snipe-enable-highlight)
                                      (evil-snipe-enable-incremental-highlight))))
      (set-transient-map evilem-map)
      (which-key-reload-key-sequence prefix))))

(remove-hook 'doom-post-init-hook #'blink-cursor-mode)
(remove-hook 'doom-post-init-hook #'shackle-mode)
(remove-hook 'doom-popup-mode-hook #'doom|hide-modeline-in-popup)
;; (defface doom-modeline-panel
;;   '((t (:inherit mode-line-highlight)))
;;   "Face for 'X out of Y' segments, such as `+doom-modeline--anzu', `+doom-modeline--evil-substitute' and
;; `iedit'"
;;   :group '+doom-modeline)

(setq doom-theme 'doom-tomorrow-night)
(defun evil-cursor-proper (&rest _)
  (setq evil-default-cursor '(t "#f0c674")
        evil-visual-state-cursor '("#81a2be" hollow)
        evil-emacs-state-cursor '("#5a5b5a" box)
        evil-replace-state-cursor '("#de935f" box)
        evil-normal-state-cursor '("#f0c674" box)
        evil-motion-state-cursor '("#b294bb" box)
        evil-insert-state-cursor '("#b5bd68" bar)))
(advice-add #'load-theme :after #'evil-cursor-proper)

;; TODO: This is to slow
;; (defvar doom-modeline-evil-state-color
;;   '((normal . "#f0c674")
;;     (insert . "#b5bd68")
;;     (emacs . "#5a5b5a")
;;     (replace . "#de935f")
;;     (visual . "#81a2be")
;;     (motion . "#b294bb")))

;; (defun doom-modeline-highlight-face-evil-state ()
;;   "Set the highlight face depending on the evil state.
;; Set `doom-modeline-highlight-face-func' to
;; `doom-modeline-highlight-face-evil-state' to use this."
;;   (if (bound-and-true-p evil-local-mode)
;;       (let* ((color (cdr (assq evil-state doom-modeline-evil-state-color))))
;;         (set-face-attribute 'doom-modeline-bar nil :background color)
;;         (set-face-attribute 'doom-modeline-panel t :background color))))

;; (advice-add 'doom-modeline-format--main :before  #'doom-modeline-highlight-face-evil-state)
;; (advice-remove 'doom-modeline-format--main  #'doom-modeline-highlight-face-evil-state)

;; (defmacro measure-time (&rest body)
;;   "Measure the time it takes to evaluate BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06f" (float-time (time-since time)))))

(require 'evil-collection-minibuffer)
(evil-collection-minibuffer-setup)
(add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
(with-eval-after-load 'calendar
  (require 'evil-collection-calendar)
  (evil-collection-calendar-setup))
(with-eval-after-load 'dired
  (require 'evil-collection-dired)
  (evil-collection-dired-setup)
  (define-key dired-mode-map " " nil)
  (evil-define-key 'normal dired-mode-map " " nil))
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
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
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
