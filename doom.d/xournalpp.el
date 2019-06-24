;;; ~/dotfiles/doom.d/xournalpp.el -*- lexical-binding: t; -*-

;;; Adapted from https://github.com/jkitchin/scimax/blob/master/scimax-inkscape.el

;;; Commentary:
;;
;; This library provides a new org-mode link for xournalpp files. When you
;; click on an xournalpp link, it will open the figure in xournalpp. A thumbnail
;; image will be placed on the xournalpp link.
;;
;; Export to HTML:
;; (browse-url (let ((org-export-before-processing-hook '(xournalpp-preprocess)))
;;   (org-html-export-to-html)))
;;
;; (org-open-file (let ((org-export-before-processing-hook '(xournalpp-preprocess)))
;;   (org-latex-export-to-pdf)))
;;
;; xournalpp does not allow you to create empty files. We save the template in a
;; variable and create them on demand.

(defcustom xournalpp-thumbnail-width 300
  "Width of thumbnails in pts."
  :group 'xournalpp)

(defcustom xournalpp-template-svg
    "<?xml version=\"1.0\" standalone=\"no\"?>
    <xournal creator=\"Xournal++ 1.0.10\" fileversion=\"4\">
    <title>Xournal++ document - see https://github.com/xournalpp/xournalpp</title>
    <page width=\"595.28\" height=\"841.89\">
    <background type=\"solid\" color=\"#ffffffff\" style=\"graph\"/>
    <layer/>
    </page>
    </xournal>"
  "Blank document for xournalpp. You cannot create a file at the
  command line, so we put this template in and open it. Works for Xournal++ 1.0.10.
  Usually xopp files are compressed, but it seems to work without compression.")

(after! org
(defun xournalpp-open (path)
  "Open the PATH in xournalpp.
Make a new file if needed."
  (interactive)
  (unless (f-ext-p path "xopp") (error "Must be an xopp file."))
  (unless (file-exists-p path)
    (with-temp-file path
      (insert xournalpp-template-svg)))

  (shell-command (format "sh -c \"xournalpp %s && xournalpp -i %s %s\" &" path (replace-regexp-in-string "xopp" "svg" path) path)))


(defun xournalpp-thumbnail (start end path bracketp)
  "Put a thumbnail on an xournalpp link."
  (let (img ov svg)
    (setq svg (replace-regexp-in-string "xopp" "svg" path))
    (when (and
	   ;; got a path
	   svg
	   ;; it is an image
	   (org-string-match-p (image-file-name-regexp) svg)
	   ;; and it exists
	   (f-exists? svg)
	   ;; and there is no overlay here.
	   (not (ov-at start)))
      (setq img (create-image
		 (expand-file-name svg)
		 'imagemagick nil :width xournalpp-thumbnail-width
		 :background "lightgray"))
      (setq ov (make-overlay start end))
      (overlay-put ov 'display img)
      (overlay-put ov 'face 'default)
      (overlay-put ov 'before-string "xournalpp:")
      (overlay-put ov 'org-image-overlay t)
      (overlay-put ov 'modification-hooks
		   (list
		    `(lambda (&rest args)
		       (org-display-inline-remove-overlay ,ov t ,start ,end))))
      (push ov org-inline-image-overlays))))


(defun xournalpp-redraw-thumbnails (&rest args)
  "Use font-lock to redraw the links."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-restart-font-lock)))

;; This gets the thumbnails to be redrawn with inline image toggling.
(advice-add 'org-display-inline-images :after 'xournalpp-redraw-thumbnails)


(defun xournalpp-preprocess (backend)
  "Preprocessing function to run in `org-export-before-processing-hook'.
Here are two examples:
 (browse-url (let ((org-export-before-processing-hook '(xournalpp-preprocess)))
  (org-html-export-to-html)))
 (org-open-file (let ((org-export-before-processing-hook '(xournalpp-preprocess)))
  (org-latex-export-to-pdf)))"
  (let ((links (reverse (org-element-map (org-element-parse-buffer) 'link
			  (lambda (link)
			    (when (string= (org-element-property :type link) "xournalpp")
			      link))))))
    (loop for link in links
	  do
	  (goto-char (org-element-property :begin link))
	  (re-search-forward "xournalpp:" (org-element-property :end link))
	  (replace-match "file:")
	  (re-search-forward "xopp" (org-element-property :end link))
	  (replace-match "svg")
      )))


(org-link-set-parameters
 "xournalpp"
 :follow 'xournalpp-open
 :help-echo "Click to open in xournalpp."
 :activate-func 'xournalpp-thumbnail
 :export (lambda (path desc backend)
	   (cond
	    ((eq 'html backend)
	     (format "<img src=\"%s\"" (replace-regexp-in-string "xopp" "svg" path)))))
 ;;  You need to use the `xournalpp-preprocess' function in a hook for
 ;; more advanced export options like captions.
 )

(defun xournalpp-insert-drawing (path)
  "Convenience function to insert a drawing with filename PATH."
  (interactive "sFilename: ")
  (insert (format "xournalpp:%s" path)))
)
(provide 'xournalpp)

;;; xournalpp.el ends here
