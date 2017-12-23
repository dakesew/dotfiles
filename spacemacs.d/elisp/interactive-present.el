;;; interactie-present.el --- Minimalist presentation minor-mode for Emacs org-mode.
;;
;; Copyright (C) 2012 by Ric Lister
;;
;; Author: Ric Lister
;; Package-Requires: ((org "7"))
;; Package-Version: 20141109.1756
;; URL: https://github.com/rlister/org-present
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;;
;;; Commentary:
;;
;; This is meant to be an extremely minimalist presentation tool for
;; Emacs org-mode.
;;
;; Usage:
;;
;; Add the following to your emacs config:
;;
;;   (add-to-list 'load-path "~/path/to/org-present")
;;   (autoload 'org-present "org-present" nil t)
;;
;;   (add-hook 'org-present-mode-hook
;;             (lambda ()
;;               (org-present-big)
;;               (org-display-inline-images)))
;;
;;   (add-hook 'org-present-mode-quit-hook
;;             (lambda ()
;;               (org-present-small)
;;               (org-remove-inline-images)))
;;
;; Open an org-mode file with each slide under a top-level heading.
;; Start org-present with org-present-mode, left and right keys will move forward
;; and backward through slides. C-c C-q will quit org-present.
;;
;; This works well with hide-mode-line (http://webonastick.com/emacs-lisp/hide-mode-line.el),
;; which hides the mode-line when only one frame and buffer are open.
;;
;; If you're on a Mac you might also want to look at the fullscreen patch here:
;; http://cloud.github.com/downloads/typester/emacs/feature-fullscreen.patch

(defvar org-interactive-present-mode-keymap (make-keymap) "org-interactive-present-mode keymap.")

;; left and right page keys
(define-key org-interactive-present-mode-keymap [next]         'org-interactive-present-next)
(define-key org-interactive-present-mode-keymap [prior]          'org-interactive-present-prev)
(define-key org-interactive-present-mode-keymap (kbd "C-c C-=") 'org-interactive-present-big)
(define-key org-interactive-present-mode-keymap (kbd "C-c C--") 'org-interactive-present-small)
(define-key org-interactive-present-mode-keymap (kbd "C-c C-q") 'org-interactive-present-quit)

;; how much to scale up font size
(defvar org-interactive-present-text-scale 5)
(defvar org-interactive-present-overlays-list nil)

(define-minor-mode org-interactive-present-mode
  "Minimalist presentation minor mode for org-mode."
  :init-value nil
  :lighter " IP"
  :keymap org-interactive-present-mode-keymap)

(make-variable-buffer-local 'org-interactive-present-mode)

(defun org-interactive-present-top ()
  "Jump to current top-level heading, should be safe outside a heading."
  (unless (org-at-heading-p) (outline-previous-heading))
  (let ((level (org-current-level)))
    (when (and level (> level 1))
      (outline-up-heading (- level 1) t))))

(defun org-interactive-present-next ()
  "Jump to next top-level heading."
  (interactive)
  (widen)
  (if (org-current-level) ;inside any heading
      (progn
	(org-interactive-present-top)
	(or
	 (org-get-next-sibling) ;next top-level heading
	 (org-interactive-present-top)))    ;if that was last, go back to top before narrow
    ;; else handle title page before first heading
    (outline-next-heading))
  (org-interactive-present-narrow))

(defun org-interactive-present-prev ()
  "Jump to previous top-level heading."
  (interactive)
  (if (org-current-level)
      (progn
	(widen)
	(org-interactive-present-top)
	(org-get-last-sibling)))
  (org-interactive-present-narrow))

(defun org-interactive-present-narrow ()
  "Show just current page; in a heading we narrow, else show title page (before first heading)."
  (if (org-current-level)
      (progn
	(org-narrow-to-subtree)
	(show-all))
    ;; else narrow to area before first heading
    (outline-next-heading)
    (narrow-to-region (point-min) (point))
    (goto-char (point-min))))

(defun org-interactive-present-beginning ()
  "Jump to first slide of presentation."
  (interactive)
  (widen)
  (beginning-of-buffer)
  (org-interactive-present-narrow))

(defun org-interactive-present-end ()
  "Jump to last slide of presentation."
  (interactive)
  (widen)
  (end-of-buffer)
  (org-interactive-present-top)
  (org-interactive-present-narrow))

(defun org-interactive-present-big ()
  "Make font size larger."
  (interactive)
  (text-scale-increase 0)
  (text-scale-increase org-interactive-present-text-scale)) ;MAKE THIS BUFFER-LOCAL

(defun org-interactive-present-small ()
  "Change font size back to original."
  (interactive)
  (text-scale-increase 0))

(defun org-interactive-present-add-overlay (beginning end)
  "Create a single overlay over given region and remember it."
  (let ((overlay (make-overlay beginning end)))
    (push overlay org-interactive-present-overlays-list)
    (overlay-put overlay 'invisible 'org-interactive-present)))

(defun org-interactive-present-show-option (string)
  "Returns non-nil if string is an org-mode exporter option whose value we want to show."
  (save-match-data
    (string-match
     (regexp-opt '("title:" "author:" "date:" "email:"))
     string)))

(defun org-interactive-present-add-overlays ()
  "Add overlays for this mode."
  (add-to-invisibility-spec '(org-interactive-present))
  (save-excursion
    ;; hide stars in headings
    (goto-char (point-min))
    (while (re-search-forward "^\\(*+\\)" nil t)
      (org-interactive-present-add-overlay (match-beginning 1) (match-end 1)))))

(defun org-interactive-present-rm-overlays ()
  "Remove overlays for this mode."
  (mapc 'delete-overlay org-interactive-present-overlays-list)
  (remove-from-invisibility-spec '(org-interactive-present)))

(defun org-interactive-present-read-write ()
  "Make buffer read-only."
  (interactive)
  (setq buffer-read-only nil)
  (setq cursor-type org-interactive-present-cursor-cache)
  (define-key org-interactive-present-mode-keymap (kbd "SPC") 'self-insert-command))

(defun org-interactive-present-hide-cursor ()
  "Hide the cursor for current window."
  (interactive)
  (internal-show-cursor (selected-window) nil))

(defun org-interactive-present-show-cursor ()
  "Show the cursor for current window."
  (interactive)
  (internal-show-cursor (selected-window) t))

;;;###autoload
(defun org-interactive-present ()
  "init."
  (interactive)
  (setq org-interactive-present-mode t)
  (org-interactive-present-add-overlays)
  (org-interactive-present-narrow)
  (org-interactive-present-big)
  (run-hooks 'org-interactive-present-mode-hook))

(defun org-interactive-present-quit ()
  "Quit the minor-mode."
  (interactive)
  (org-interactive-present-small)
  (org-interactive-present-rm-overlays)
  (widen)
  (org-interactive-present-small)
  (run-hooks 'org-interactive-present-mode-quit-hook)
  (setq org-interactive-present-mode nil))

(provide 'org-interactive-present)
