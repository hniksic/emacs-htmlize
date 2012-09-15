;;; htmlize.el -- HTML-ize font-lock buffers

;; Copyright (c) 1997 Free Software Foundation

;; Author: Hrvoje Niksic <hniksic@srce.hr>
;; Keywords: hypermedia, extensions
;; Version: 0.7

;; This file is not yet part of any Emacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: not in FSF

;;; Commentary:

;; This package will allow you to HTML-ize your font-lock buffers.  It
;; takes into account only the colors.  A lot of functionality could
;; be added.  Mail to <hniksic@srce.hr> to discuss features and
;; additions.  All suggestions are more than welcome.

;; This package generates correct HTML (or a semblance of it; I
;; haven't yet bothered to actually run it through a checker).  Since
;; <font> is not allowed to be within <pre>, we cheat by inserting the
;; DTD for HTML Pro.  Ha ha ha.

;; To use, just go to a buffer, and invoke `M-x htmlize-buffer', and
;; you'll be put to an HTML buffer, which you can save.  The operation
;; can take a bit of time, if your original buffer is long -- so be
;; patient.

;; This code should work under XEmacs 19.14+ and GNU Emacs 19.34+.

;; Useful additions by Ron Gut <rgut@aware.com> incorporated.


;;; Code:

(require 'cl)


;; BLOB to make custom stuff work even without customize
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

(defgroup htmlize nil
  "HTMLize font-locked buffers."
  :group 'hypermedia)

(defcustom htmlize-tags ""
  "*Headers to insert."
  :type 'string
  :group 'htmlize)

;; We use the HTML Pro DTD by default.  Note that under any other DTD
;; it is illegal to specify <font> under <pre>.
(defcustom htmlize-dtd-version
  "HTML PUBLIC \"+//Silmaril//DTD HTML Pro v0r11 19970101//EN\">\n"
  "*Doctype of created HTMLs.
Set this to the value of `html-helper-htmldtd-version' for consistency
with psgml-html."
  :type 'string
  :group 'htmlize)

(defvar htmlize-character-table
  (let ((table (make-vector 256 ?\0)))
    (dotimes (i 256)
      (setf (aref table i) (char-to-string i)))
    (setf (aref table ?&) "&amp;"
	  (aref table ?<) "&lt;"
	  (aref table ?>) "&gt;"
	  (aref table ?\") "&quot;")
    table))

(defun htmlize-protect (string)
  (mapconcat (lambda (char)
	       (aref htmlize-character-table char))
	     string ""))

(defsubst htmlize-face-color (face &optional bg-p)
  (if (fboundp 'color-instance-rgb-components)
      (mapcar (lambda (arg)
		(/ arg 256))
	      (color-instance-rgb-components
	       (if bg-p
		   (face-background-instance face)
		 (face-foreground-instance face))))
    (mapcar (lambda (arg)
	      (/ arg 256))
	    (x-color-values
	     (or (if bg-p
		     (face-background face)
		   (face-foreground face))
		 (if bg-p "white"
		   "black"))))))

(defsubst htmlize-face-color-string (face &optional bg-p)
  (apply 'format "#%02x%02x%02x" (htmlize-face-color face bg-p)))

;; `insert-string' is useful in XEmacs.
(if (string-match "XEmacs" emacs-version)
    (defalias 'htmlize-insert-string 'insert-string)
  (defun htmlize-insert-string (str buf)
    (letf (((current-buffer) buf))
      (insert str))))

;;;###autoload
(defun htmlize-buffer (&optional buffer)
  "HTML-ize BUFFER."
  (interactive)
  (or buffer
      (setq buffer (current-buffer)))
  (let ((newbuf (get-buffer-create "*html*"))
	plist next-change face color-name)
    (save-excursion
      (set-buffer newbuf)
      (erase-buffer)
      (insert
       "<!DOCTYPE "
       "HTML PUBLIC \"+//Silmaril//DTD HTML Pro v0r11 19970101//EN\">\n"
       "<html>\n<head>\n<title>"
       (if (stringp buffer) buffer
	 (buffer-name buffer))
       "</title>\n" htmlize-tags
       "</head>\n"
       (format "<body bgcolor=\"%s\" text=\"%s\">\n"
	       (htmlize-face-color-string 'default t)
	       (htmlize-face-color-string 'default))
       "<pre>\n")
      (set-buffer buffer)
      (goto-char (point-min))
      (while (not (eobp))
	(setq plist (text-properties-at (point))
	      next-change (or (next-property-change (point) (current-buffer))
			      (point-max)))
	(setq color-name nil)
	(setq face (plist-get plist 'face))
	(when face
	  (and (consp face)
	       ;; Choose the first face.
	       (setq face (car face)))
	  (setq color-name (htmlize-face-color-string face))
	  (htmlize-insert-string
	   (concat "<font color=\"" color-name "\">") newbuf))
	(htmlize-insert-string (htmlize-protect
				(buffer-substring (point) next-change))
			       newbuf)
	(when color-name
	  (htmlize-insert-string "</font>" newbuf))
	(goto-char next-change)))
    (switch-to-buffer newbuf)
    (insert "</pre>\n</body>\n</html>\n")
    (goto-char (point-min))))

(provide 'htmlize)

;;; htmlize.el ends here
