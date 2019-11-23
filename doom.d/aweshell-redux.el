;;; From https://github.com/manateelazycat/aweshell
;;; aweshell.el --- Awesome eshell

;; Filename: aweshell.el
;; Description: Awesome eshell
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-13 23:18:35
;; Version: 4.3
;; Last-Updated: 2019-07-17 11:58:45
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/aweshell.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `eshell' `eshell-prompt-extras' `exec-path-from-shell' `cl-lib' `subr-x'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; I created `multi-term.el' and use it many years.
;; Now I'm a big fans of `eshell'.
;;
;; So i write `aweshell.el' to extension `eshell' with below features:
;; 1. Create and manage multiple eshell buffers.
;; 2. Add some useful commands, such as: clear buffer, toggle sudo etc.
;; 3. Display extra information and color like zsh, powered by `eshell-prompt-extras'
;; 4. Add Fish-like history autosuggestions.
;; 5. Validate and highlight command before post to eshell.
;; 6. Change buffer name by directory change.
;; 7. Add completions for git command.
;; 8. Fix error `command not found' in MacOS.
;; 9. Integrate `eshell-up'.
;; 10. Unpack archive file.
;; 11. Open file with alias e.
;; 12. Output "did you mean ..." helper when you typo.
;; 13. Make cat file with syntax highlight.
;; 14. Alert user when background process finished or aborted.
;; 15. Complete shell command arguments like IDE feeling.
;;

;;; Installation:
;;
;; Put `aweshell.el', `eshell-prompt-extras.el', `exec-path-from-shell.el' to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'aweshell)
;;
;; Binding your favorite key to functions:
;;
;; `aweshell-new'
;; `aweshell-next'
;; `aweshell-prev'
;; `aweshell-clear-buffer'
;; `aweshell-sudo-toggle'
;; `aweshell-switch-buffer'
;;

;;; Customize:
;;
;; `aweshell-complete-selection-key'
;; `aweshell-clear-buffer-key'
;; `aweshell-sudo-toggle-key'
;; `aweshell-search-history-key'
;;
;; `aweshell-valid-command-color'
;; `aweshell-invalid-command-color'
;; `aweshell-use-exec-path-from-shell'
;;
;; All of the above can customize by:
;;      M-x customize-group RET aweshell RET
;;

;;; Change log:
;;;
;;
;; 2019/09/12
;;      * Use `cl-lib' instead of `cl'
;;
;; 2019/07/17
;;      * Fix #37 issue: aweshell-dedicated-toggle failed after user use cd command in aweshell.
;;
;; 2019/07/16
;;      * Add `aweshell-dedicated-toggle' command.
;;      * Refactory code.
;;
;; 2019/04/29
;;      * Add interactive command `aweshell-switch-buffer'.
;;
;; 2019/04/07
;;      * Increased startup speed, loaded `eshell-did-you-mean' plugin when idle
;;
;; 2019/1/2
;;      * When the command includes * or \ , the `pcomplete-completions' command will report an error,
;;        So company menu will be disabled when these characters are included.
;;
;; 2018/12/28
;;      * When the command includes " or [ , the `pcomplete-completions' command will report an error,
;;        So company menu will be disabled when these characters are included.
;;
;; 2018/12/27
;;      * Fix backtrace when type command: git clone "
;;
;; 2018/12/22
;;      * Mix best history and complete arguments just when history not exist in completion arguments.
;;
;; 2018/12/15
;;      * Mix best match history and shell completions in company's completion menu.
;;      * Fix error when completion.
;;
;; 2018/12/14
;;      * Add new option `aweshell-autosuggest-backend' to swtich between fish-style and company-style.
;;
;; 2018/11/12
;;      * Remove Mac color, use hex color instead.
;;
;; 2018/10/19
;;      * Alert user when background process finished or aborted.
;;
;; 2018/09/19
;;      * Make `exec-path-from-shell' optional. Disable with variable`aweshell-use-exec-path-from-shell'.
;;
;; 2018/09/17
;;      * Use `ido-completing-read' instead `completing-read' to provide fuzz match.
;;
;; 2018/09/10
;;      * Built-in `eshell-did-you-mean' plugin.
;;
;; 2018/09/07
;;      * Add docs about `eshell-up', `aweshell-emacs' and `aweshell-unpack'
;;      * Add `aweshell-cat-with-syntax-highlight' make cat file with syntax highlight.
;;
;; 2018/09/06
;;      * Require `cl' to fix function `subseq' definition.
;;
;; 2018/08/16
;;      * Just run git relative code when git in exec-path.
;;      * Use `esh-parse-shell-history' refacotry code.
;;      * Try to fix error "Shell command failed with code 1 and no output" cause by LANG environment variable.
;;
;; 2018/08/15
;;      * Remove face settings.
;;      * Add `aweshell-search-history' and merge bash/zsh history in `esh-autosuggest' .
;;      * Fix history docs.
;;
;; 2018/08/14
;;      * Save buffer in `aweshell-buffer-list', instead save buffer name.
;;      * Change aweshell buffer name by directory change.
;;      * Refacotry code.
;;      * Fix error "wrong-type-argument stringp nil" by `aweshell-validate-command'
;;      * Add some handy aliases.
;;      * Make `aweshell-validate-command' works with eshell aliases.
;;      * Synchronal buffer name with shell path by `epe-fish-path'.
;;      * Use `epe-theme-pipeline' as default theme.
;;      * Complete customize options in docs.
;;      * Redirect `clear' alias to `aweshell-clear-buffer'.
;;      * Add completions for git command.
;;      * Adjust `ls' alias.
;;
;; 2018/08/13
;;      * First released.
;;

;;; Acknowledgements:
;;
;; Samray: copy `aweshell-clear-buffer', `aweshell-sudo-toggle' and `aweshell-search-history'
;; casouri: copy `aweshell-validate-command' and `aweshell-sync-dir-buffer-name'
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'eshell)
(require 'cl-lib)
(require 'subr-x)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar aweshell-use-exec-path-from-shell t)

(when (and aweshell-use-exec-path-from-shell
           (featurep 'cocoa))
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup aweshell nil
  "Multi eshell manager."
  :group 'aweshell)

(defcustom aweshell-complete-selection-key "M-h"
  "The keystroke for complete history auto-suggestions."
  :type 'string
  :group 'aweshell)

(defcustom aweshell-clear-buffer-key "C-l"
  "The keystroke for clear buffer."
  :type 'string
  :group 'aweshell)

(defcustom aweshell-sudo-toggle-key "C-S-l"
  "The keystroke for toggle sudo"
  :type 'string
  :group 'aweshell)

(defcustom aweshell-search-history-key "M-'"
  "The keystroke for search history"
  :type 'string
  :group 'aweshell)

(defcustom aweshell-valid-command-color "#98C379"
  "The color of valid command by `aweshell-validate-command'."
  :type 'string
  :group 'aweshell)

(defcustom aweshell-invalid-command-color "#FF0000"
  "The color of invalid command by `aweshell-validate-command'."
  :type 'string
  :group 'aweshell)

(defface aweshell-alert-buffer-face
  '((t (:foreground "#ff2d55" :bold t)))
  "Alert buffer face."
  :group 'aweshell)

(defface aweshell-alert-command-face
  '((t (:foreground "#ff9500" :bold t)))
  "Alert command face."
  :group 'aweshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar aweshell-buffer-list nil
  "The list of non-dedicated eshell buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hook ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'kill-buffer-hook 'aweshell-kill-buffer-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aweshell-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'eshell-mode)
    (let ((killed-buffer (current-buffer)))
      (setq aweshell-buffer-list
            (delq killed-buffer aweshell-buffer-list)))))

(defun aweshell-get-buffer-index ()
  (let ((eshell-buffer-index-list (aweshell-get-buffer-index-list))
        (eshell-buffer-index-counter 1))
    (if eshell-buffer-index-list
        (progn
          (dolist (buffer-index eshell-buffer-index-list)
            (if (equal buffer-index eshell-buffer-index-counter)
                (setq eshell-buffer-index-counter (+ 1 eshell-buffer-index-counter))
              (return eshell-buffer-index-counter)))
          eshell-buffer-index-counter)
      1)))

(defun aweshell-get-buffer-names ()
  (let (eshell-buffer-names)
    (dolist (frame (frame-list))
      (dolist (buffer (buffer-list frame))
        (with-current-buffer buffer
          (if (eq major-mode 'eshell-mode)
              (add-to-list 'eshell-buffer-names (buffer-name buffer))))))
    eshell-buffer-names))

(defun aweshell-get-buffer-index-list ()
  (let ((eshell-buffer-names (aweshell-get-buffer-names)))
    (if eshell-buffer-names
        (let* ((eshell-buffer-index-strings
                (seq-filter (function
                             (lambda (buffer-index)
                               (and (stringp buffer-index)
                                    (not (equal 0 (string-to-number buffer-index))))))
                            (mapcar (function
                                     (lambda (buffer-name)
                                       (if (integerp (string-match "\\*eshell\\*\<\\([0-9]+\\)\>" buffer-name))
                                           (subseq buffer-name (match-beginning 1) (match-end 1))
                                         nil)))
                                    eshell-buffer-names)))
               (eshell-buffer-index-list (sort (seq-map 'string-to-number eshell-buffer-index-strings) '<)))
          eshell-buffer-index-list)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aweshell-toggle (&optional arg)
  "Toggle Aweshell.
If called with prefix argument, open Aweshell buffer in current directory when toggling on Aweshell. If there exists an Aweshell buffer with current directory, use that, otherwise create one."
  (interactive "p")
  (if (equal major-mode 'eshell-mode)
      ;; toggle off
      (while (equal major-mode 'eshell-mode)
        (switch-to-prev-buffer))
    ;; toggle on
    (if (eq arg 4)
        ;; open in current dir
        (let* ((dir default-directory)
               (existing-buffer
                (catch 'found
                  (dolist (aweshell-buffer aweshell-buffer-list)
                    (with-current-buffer aweshell-buffer
                      (when (equal dir default-directory)
                        (throw 'found aweshell-buffer)))))))
          ;; found the buffer with the same dir
          ;; or create a new one
          (if existing-buffer
              (switch-to-buffer existing-buffer)
            (message "No Aweshell buffer with current dir found, creating a new one.")
            (switch-to-buffer (car (last (aweshell-new))))
            (eshell/cd dir)))
      ;; simply open
      (aweshell-next))))

(defun aweshell-new ()
  "Create new eshell buffer."
  (interactive)
  (setq aweshell-buffer-list (nconc aweshell-buffer-list (list (eshell (aweshell-get-buffer-index))))))

(defun aweshell-next ()
  "Select next eshell buffer.
Create new one if no eshell buffer exists."
  (interactive)
  (if (or (not aweshell-buffer-list) (equal (length aweshell-buffer-list) 0))
      (aweshell-new)
    (let* ((current-buffer-index (cl-position (current-buffer) aweshell-buffer-list))
           (switch-index (if current-buffer-index
                             (if (>= current-buffer-index (- (length aweshell-buffer-list) 1))
                                 0
                               (+ 1 current-buffer-index))
                           0)))
      (switch-to-buffer (nth switch-index aweshell-buffer-list))
      )))

(defun aweshell-prev ()
  "Select previous eshell buffer.
Create new one if no eshell buffer exists."
  (interactive)
  (if (or (not aweshell-buffer-list) (equal (length aweshell-buffer-list) 0))
      (aweshell-new)
    (let* ((current-buffer-index (cl-position (current-buffer) aweshell-buffer-list))
           (switch-index (if current-buffer-index
                             (if (<= current-buffer-index 0)
                                 (- (length aweshell-buffer-list) 1)
                               (- current-buffer-index 1))
                           (- (length aweshell-buffer-list) 1))))
      (switch-to-buffer (nth switch-index aweshell-buffer-list))
      )))

(defun aweshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun aweshell-sudo-toggle ()
  "Toggle sudo with current command."
  (interactive)
  (save-excursion
    (let ((commands (buffer-substring-no-properties
                     (eshell-bol) (point-max))))
      (if (string-match-p "^sudo " commands)
          (progn
            (eshell-bol)
            (while (re-search-forward "sudo " nil t)
              (replace-match "" t nil)))
        (progn
          (eshell-bol)
          (insert "sudo ")
          )))))

(defun aweshell-search-history ()
  "Interactive search eshell history."
  (interactive)
  (save-excursion
    (let* ((start-pos (eshell-beginning-of-input))
           (input (eshell-get-old-input))
           (all-shell-history (aweshell-parse-shell-history)))
      (let* ((command (ido-completing-read "Search history: " all-shell-history)))
        (eshell-kill-input)
        (insert command)
        )))
  ;; move cursor to eol
  (end-of-line))

(defun aweshell-switch-buffer ()
  "Switch to another aweshell buffer."
  (interactive)
  (cond ((= 0 (length aweshell-buffer-list))
         (aweshell-new)
         (message "No Aweshell buffer yet, create a new one."))
        ((= 1 (length aweshell-buffer-list)) ; only one Aweshell buffer, just switch to it
         (switch-to-buffer (nth 0 aweshell-buffer-list)))
        (t
         (let* ((completion-extra-properties '(:annotation-function aweshell-switch-buffer--annotate))
                (buffer-alist (mapcar (lambda (buffer) `(,(buffer-name buffer) . ,buffer)) aweshell-buffer-list))
                (pwd default-directory)
                (preselect))
           ;; find most suitable preselect buffer
           (dolist (buffer aweshell-buffer-list)
             (with-current-buffer buffer
               (when (and
                      (or (not preselect) (< (length preselect) (length default-directory)))
                      (file-in-directory-p pwd default-directory))
                 (setq preselect (propertize default-directory :buffer-name (buffer-name buffer))))))
           (let ((result-buffer (completing-read "Switch to Aweshell buffer: " buffer-alist nil t nil nil
                                                 (get-text-property 0 :buffer-name (or preselect "")))))
             (switch-to-buffer (alist-get result-buffer buffer-alist nil nil #'equal)))))))

(defun aweshell-switch-buffer--annotate (candidate)
  (let* ((buffer-alist
          (mapcar (lambda (buffer) `(,(buffer-name buffer) . ,buffer)) aweshell-buffer-list))
         (candidate-buffer (alist-get candidate buffer-alist nil nil #'equal)))
    (with-current-buffer candidate-buffer
      ;; display the last command of aweshell buffer
      (format "  <%s> %s" (eshell-get-history 0) (if eshell-current-command "(Running)" "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Aweshell keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd aweshell-clear-buffer-key) 'aweshell-clear-buffer)
            (define-key eshell-mode-map (kbd aweshell-sudo-toggle-key) 'aweshell-sudo-toggle)
            (define-key eshell-mode-map (kbd aweshell-search-history-key) 'aweshell-search-history)
            ))

(defun aweshell-emacs (&rest args)
  "Open a file in Emacs with ARGS, Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defalias 'eshell/e 'aweshell-emacs)

(defun aweshell-unpack (file &rest args)
  "Unpack FILE with ARGS."
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not unpack the file:'")))))
    (let ((unpack-command(concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))
    ))

(defalias 'eshell/unpack 'aweshell-unpack)

;; Synchronal buffer name by directory change.
(defun aweshell-sync-dir-buffer-name ()
  "Change aweshell buffer name by directory change."
  (when (equal major-mode 'eshell-mode)
    (rename-buffer (format "Aweshell: %s" (epe-fish-path default-directory))
                   t)))
;; https://www.emacswiki.org/emacs/EshellPrompt
(defun epe-fish-path (path)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (max-len 30)
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(add-hook 'eshell-directory-change-hook #'aweshell-sync-dir-buffer-name)
(add-hook 'eshell-mode-hook #'aweshell-sync-dir-buffer-name)

;; Make cat with syntax highlight.
(defun aweshell-cat-with-syntax-highlight (filename)
  "Like cat(1) but with syntax highlighting."
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (buffer-string)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(advice-add 'eshell/cat :override #'aweshell-cat-with-syntax-highlight)

;; Alert user when background process finished or aborted.
(defun eshell-command-alert (process status)
  "Send `alert' with severity based on STATUS when PROCESS finished."
  (let* ((cmd (process-command process))
         (buffer (process-buffer process))
         (msg (replace-regexp-in-string "\n" " " (string-trim (format "%s: %s" (mapconcat 'identity cmd " ")  status))))
         (buffer-visible (member buffer (mapcar #'window-buffer (window-list)))))
    (unless buffer-visible
      (message "%s %s"
               (propertize (format "[Aweshell Alert] %s" (string-remove-prefix "Aweshell: " (buffer-name buffer))) 'face 'aweshell-alert-buffer-face)
               (propertize msg 'face 'aweshell-alert-command-face)))))

(add-hook 'eshell-kill-hook #'eshell-command-alert)

(provide 'aweshell-redux)

;;; aweshell.el ends here

