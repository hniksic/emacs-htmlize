;; htmlize.el -- HTML-ize font-lock buffers

;; Copyright (C) 1997,1998,1999,2000,2001,2002,2003 Hrvoje Niksic

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: hypermedia, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package analyses the text properties of the buffer and
;; converts them, along with the text, to HTML.  Mail to
;; <hniksic@xemacs.org> to discuss features and additions.  All
;; suggestions are more than welcome.

;; To use this, just switch to the buffer you want HTML-ized and type
;; `M-x htmlize-buffer'.  You will be switched to a new buffer that
;; contains the resulting HTML code.  You can edit and inspect this
;; buffer, or you can just save it with C-x C-w.  `M-x htmlize-file'
;; will find a file, fontify it, and save the HTML version in
;; FILE.html, without any additional intervention.  `M-x
;; htmlize-many-files' allows you to htmlize any number of files in
;; the same manner.  `M-x htmlize-many-files-dired' does the same for
;; files marked in a dired buffer.

;; htmlize supports two types of HTML output, selected by setting
;; `htmlize-output-type': `css' and `font'.  In `css' mode, htmlize
;; uses cascading style sheets to specify colors; it generates classes
;; that correspond to Emacs faces and uses <span clas=FACE>...</span>
;; to color parts of text.  In this mode, the produced HTML is valid
;; under the 4.01 strict DTD, as confirmed by the W3C validator.  In
;; `font' mode, htmlize uses <font color="...">...</font> to colorize
;; HTML, which is not standard-compliant, but works better in older
;; browsers.  `css' mode is the default.

;; You can also use htmlize from your Emacs Lisp code.  When called
;; non-interactively, `htmlize-buffer' and `htmlize-region' will
;; return the resulting HTML buffer, but will not switch current
;; buffer or move the point.

;; I tried to make the package elisp-compatible with multiple Emacsen,
;; specifically aiming for XEmacs 19.14+ and GNU Emacs 19.34+.  Please
;; let me know if it doesn't work on some of those, and I'll try to
;; fix it.  I relied heavily on the presence of CL extensions,
;; especially for cross-emacs compatibility; please don't try to
;; remove that particular dependency.  When byte-compiling under GNU
;; Emacs, you're likely to get lots of warnings; just ignore them.

;; The latest version should be available at:
;;
;;        <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el>
;;
;; You can find a sample of htmlize's output (possibly generated with
;; an older version) at:
;;
;;        <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.html>
;;

;; Thanks go to:
;;   * Ron Gut <rgut@aware.com>, for useful additions (hooks and
;;     stuff);
;;
;;   * Bob Weiner <weiner@altrasoft.com>, for neat ideas (use of
;;     rgb.txt and caching color strings);
;;
;;   * Toni Drabik <tdrabik@public.srce.hr>, for a crash course to
;;     CSS1.
;;
;;   * Peter Breton <pbreton@ne.mediaone.net>, for useful suggestions
;;     (multiple file stuff) and dired code.
;;
;;   * Thomas Vogels <tov@ece.cmu.edu> and Juanma Barranquero
;;     <barranquero@laley-actualidad.es> for contributing fixes.
;;
;;   * A bunch of other people for sending reports and useful
;;     comments.  I will not attempt to name them because I will
;;     surely forget some.
;;

;; User quotes: "You sir, are a sick, sick, _sick_ person. :)"
;;                  -- Bill Perry, author of Emacs/W3


;;; Code:

(require 'cl)
(eval-when-compile
  (if (string-match "XEmacs" emacs-version)
      (byte-compiler-options
	(warnings (- unresolved))))
  (defvar font-lock-auto-fontify)
  (defvar global-font-lock-mode))

(defconst htmlize-version "1.4")

;; Incantations to make custom stuff work without customize, e.g. on
;; XEmacs 19.14 or GNU Emacs 19.34.
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
      (` (defvar (, var) (, value) (, doc))))
    (defmacro defface (face value doc &rest stuff)
      `(make-face ,face))))

(defgroup htmlize nil
  "HTMLize font-locked buffers."
  :group 'hypermedia)

(defcustom htmlize-head-tags ""
  "*Additional tags to insert within HEAD of the generated document."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-output-type 'css
  "*Output type of generated HTML.  Legal values are `css' and `font'.
When set to `css' (the default), htmlize will generate a style sheet
with description of faces, and use it in the HTML document, specifying
the faces in the actual text with <span class=\"FACE\">.

When set to `font', the properties will be set using layout tags
<font>, <b>, <i>, <u>, and <strike>.

`css' output is normally preferred, but `font' is still useful for
supporting old, pre-CSS browsers, or for easy embedding of colorized
text in foreign HTML documents (no style sheet to carry around)."
  :type '(choice (const css) (const font))
  :group 'htmlize)

(defcustom htmlize-generate-hyperlinks t
  "*Non-nil means generate the hyperlinks for URLs and mail addresses.
This is on by default; set it to nil if you don't want htmlize to
insert hyperlinks in the resulting HTML.  (In which case you can still
do your own hyperlinkification from htmlize-after-hook.)"
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-hyperlink-style "\
      a {
        color: inherit;
        background-color: inherit;
        font: inherit;
        text-decoration: inherit;
      }
      a:hover {
        text-decoration: underline;
      }
"
  "*The CSS style used for hyperlinks when in CSS mode."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-html-charset nil
  "*The charset declared by the resulting HTML documents.
The W3C validator requires valid HTML documents to declare a charset
in a number of ways, the META tag being the only one available to
htmlize.  Therefore, when this variable is non-nil, htmlize inserts
the following in the <head> section of the HTML:

  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=CHARSET\">

where CHARSET is the value you've set for htmlize-html-charset.  Valid
charsets are defined by MIME and include strings like \"iso-8859-1\",
\"iso-8859-15\", \"utf-8\", etc.

Needless to say, if you set this, you should actually make sure that
the buffer is in the encoding you're claiming it is in.  (Under Mule
that is done by ensuring the correct \"file coding system\" for the
buffer.)  If you don't understand what that means, this option is
probably not for you."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-css-name-prefix ""
  "*The prefix to use for CSS names.

The CSS names that htmlize generates from face names are often too
generic for CSS files; for example, `font-lock-type-face' is transformed
to `type'.  Use this variable to add a prefix to the generated names.
The string \"htmlize-\" is an example of a reasonable prefix."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-use-rgb-txt t
  "*Whether `rgb.txt' should be used to convert color names to RGB.

This conversion means determining, for instance, that the color
\"IndianRed\" corresponds to the (205, 92, 92) RGB triple.  `rgb.txt'
is the X color database that maps hundreds of color names to such RGB
triples.  When this variable is non-nil, `htmlize' uses `rgb.txt' to
look up color names.

If this variable is nil, htmlize queries Emacs for RGB components of
colors using `color-instance-rgb-components' and `x-color-values'.
This can yield incorrect results on non-true-color displays.

If the `rgb.txt' file is not found (which will be the case if you're
running Emacs on non-X11 systems), this option is ignored."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-html-major-mode nil
  "The mode the newly created HTML buffer will be put in.
Set this to nil if you prefer the default (fundamental) mode."
  :type '(radio (const :tag "No mode (fundamental)" nil)
		 (function-item html-mode)
		 (function :tag "User-defined major mode"))
  :group 'htmlize)

(defvar htmlize-before-hook nil
  "Hook run before htmlizing a buffer.
The hook is run in the original buffer (not HTML buffer), so you may
wish to add `font-lock-fontify-buffer' here.")

(defvar htmlize-after-hook nil
  "Hook run after htmlizing a buffer.
Unlike `htmlize-before-hook', these functions are run in the HTML
buffer.  You may use them to modify the outlook of the final HTML
output.")

(defvar htmlize-file-hook nil
  "Hook run after htmlizing a file, and before writing it out to disk.
This is run by the `htmlize-file'.")

;; I try to conditionalize on features rather than Emacs version, but
;; in some cases checking against the version *is* necessary.
(defconst htmlize-running-xemacs (string-match "XEmacs" emacs-version))


;;; Transformation of buffer text: HTML escapes, untabification, etc.

(if (fboundp 'char-int)
    (defalias 'htmlize-char-int 'char-int)
  (defalias 'htmlize-char-int 'identity))

(defvar htmlize-character-table
  ;; Map characters in the 0-255 range to strings.
  (let ((table (make-vector 256 ?\0)))
    ;; Map characters in the 32-126 range to themselves, others to
    ;; &#CODE entities;
    (dotimes (i 256)
      (setf (aref table i) (if (and (>= i 32) (<= i 126))
			       (char-to-string i)
			     (format "&#%d;" i))))
    ;; Set exceptions manually.
    (setf
     ;; Don't quote newline, carriage return, and TAB.
     (aref table ?\n) "\n"
     (aref table ?\r) "\r"
     (aref table ?\t) "\t"
     ;; Encode &, <, and > as symbolic entities, as is customary.
     (aref table ?&) "&amp;"
     (aref table ?<) "&lt;"
     (aref table ?>) "&gt;"
     ;; Not quoting '"' buys us a measurable speed increase.  It's
     ;; only necessary to quote it for strings used in attribute
     ;; values, which htmlize doesn't do.
     ;(aref table ?\") "&quot;"
     )
    table))

;; Table that maps extended characters to their numeric Unicode
;; entities.  This is used by htmlize-protect-string to avoid consing
;; "&CHAR-CODE;" strings for the characters that repeat over and over.
(defvar htmlize-extended-character-table (make-hash-table :test 'eq))

(defun htmlize-protect-string (string)
  "HTML-protect string, escaping HTML metacharacters and I18N chars."
  ;; Only protecting strings that actually contain unsafe chars
  ;; removes a lot of unnecessary consing.
  (if (not (string-match "[^\r\n\t -%'-;=?-~]" string))
      string
    (mapconcat (lambda (char)
		 (cond
		  ((> (htmlize-char-int char) 255)
		   (if (and (fboundp 'encode-char)
			    ;; Emacs's unicode tables are incomplete;
			    ;; encode-char returns nil for Arabic.
			    (encode-char char 'ucs))
		       ;; encode-char is available: convert CHAR to
		       ;; "&#UCS-CODE;".  Cache the resulting string
		       ;; in htmlize-extended-character-table, so we
		       ;; don't have to cons new strings for chars
		       ;; we've already seen.
		       (or (gethash char htmlize-extended-character-table)
			   (setf (gethash char
					  htmlize-extended-character-table)
				 (format "&#%d;" (encode-char char 'ucs))))
		     ;; Conversion to Unicode not available --
		     ;; simply copy the char unchanged.
		     (char-to-string char)))
		  (t
		   ;; Use htmlize-character-table to convert CHAR to
		   ;; string without consing a new string each time.
		   (aref htmlize-character-table char))))
	       string "")))

;; We need a function that efficiently finds the next change of a
;; property (usually `face'), preferably regardless of whether the
;; change occurred because of a text property or an extent/overlay.
;; As it turns out, it is not easy to do that compatibly.

;; Under XEmacs, `next-single-property-change' does that.  Under GNU
;; Emacs beginning with version 21, `next-single-char-property-change'
;; is available and works.  GNU Emacs 20 had
;; `next-char-property-change', which we can use.  GNU Emacs 19 didn't
;; provide any means for simultaneously examining overlays and text
;; properties, so when using Emacs 19.34, we punt and fall back to
;; `next-single-property-change', thus ignoring overlays altogether.

(cond
 (htmlize-running-xemacs
  ;; XEmacs: good.
  (defun htmlize-next-change (pos prop &optional limit)
    (next-single-property-change pos prop nil (or limit (point-max)))))
 ((fboundp 'next-single-char-property-change)
  ;; GNU Emacs 21: good.
  (defun htmlize-next-change (pos prop &optional limit)
    (next-single-char-property-change pos prop nil limit)))
 ((fboundp 'next-char-property-change)
  ;; GNU Emacs 20: bad, but fixable.
  (defun htmlize-next-change (pos prop &optional limit)
    (let ((done nil)
	  (current-value (get-char-property pos prop))
	  newpos next-value)
      ;; Loop over positions returned by next-char-property-change
      ;; until the value of PROP changes or we've hit EOB.
      (while (not done)
	(setq newpos (next-char-property-change pos limit)
	      next-value (get-char-property newpos prop))
	(cond ((eq newpos pos)
	       ;; Possibly at EOB?  Whatever, just don't infloop.
	       (setq done t))
	      ((eq next-value current-value)
	       ;; PROP hasn't changed -- keep looping.
	       )
	      (t
	       (setq done t)))
	(setq pos newpos))
      pos)))
 (t
  ;; GNU Emacs 19.34: hopeless, cannot properly support overlays.
  (defun htmlize-next-change (pos prop &optional limit)
    (unless limit
      (setq limit (point-max)))
    (let ((res (next-single-property-change pos prop)))
      (if (or (null res)
	      (> res limit))
	  limit
	res)))))

(defun htmlize-buffer-substring (beg end)
  ;; Like buffer-substring-no-properties, but also ignores invisible
  ;; text.

  ;; Iterate over the changes in the `invisible' property and filter
  ;; out the portions where it's non-nil, i.e. where the text is
  ;; invisible.
  (let ((pos beg)
	visible-list invisible next-change)
    (while (< pos end)
      (setq invisible (get-char-property pos 'invisible)
	    next-change (htmlize-next-change pos 'invisible end))
      (unless invisible
	(push (buffer-substring-no-properties pos next-change)
	      visible-list))
      (setq pos next-change))
    (apply #'concat (nreverse visible-list))))

(defun htmlize-untabify-1 (line start-column)
  ;; Replaces tabs in LINE with the number of spaces sufficient to
  ;; reach the next tabstop.  The tabstops are positioned at locations
  ;; proportional to tab-width -- e.g. 0, 8, 16, etc. for tab-width 8.
  ;; This works correctly only for single-line strings; for a
  ;; multiline interface, see htmlize-untabify.
  (while (string-match "\t" line)
    (let* ((tabpos (match-beginning 0))
	   (column (+ start-column tabpos))
	   (tabsize (- tab-width (% column tab-width))))
      ;; Replace the tab with TABSIZE spaces.
      (setq line (concat (substring line 0 tabpos)
			 (make-string tabsize ?\ )
			 (substring line (1+ tabpos))))))
  line)

(defun htmlize-untabify (text start-column)
  "Untabify TEXT, assuming it starts at START-COLUMN."
  ;; Since htmlize-untabify-1 works only on single lines, iterate the
  ;; string line by line and untabify each line.  It's possible to
  ;; rewrite htmlize-untabify-1 to work with multiple-line strings,
  ;; but that function conses four strings for each tab and becomes
  ;; really slow with large inputs.  Therefore it's actually a good
  ;; idea to feed it smaller chunks.
  (let ((output nil)
	(line-beg 0)
	(textlen (length text)))
    (while (< line-beg textlen)
      (let* ((line-end (or (and (string-match "\n" text line-beg)
				(1+ (match-beginning 0)))
			   textlen))
	     (line (substring text line-beg line-end)))
	;; Untabify the line and push it to OUTPUT.
	(push (htmlize-untabify-1 line start-column) output)
	;; START-COLUMN is only valid for the first line.
	(setq start-column 0)
	;; Advance to the next position in TEXT.
	(setq line-beg line-end)))
    (apply #'concat (nreverse output))))

;;; Color handling.

(if (fboundp 'locate-file)
    (defalias 'htmlize-locate-file 'locate-file)
  (defun htmlize-locate-file (file path)
    (dolist (dir path nil)
      (when (file-exists-p (expand-file-name file dir))
	(return (expand-file-name file dir))))))

(if (fboundp 'file-name-extension)
    (defalias 'htmlize-file-name-extension 'file-name-extension)
  (defun htmlize-file-name-extension (filename &optional period)
    (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
      (and (string-match "\\.[^.]*\\'" file)
	   (substring file (+ (match-beginning 0) (if period 0 1)))))))

(eval-and-compile
  ;; I hate having replacement macros which are not colorized or
  ;; indented properly, so I'll just define save-current-buffer and
  ;; with-current-buffer if I can't find them.  htmlize is hardly a
  ;; package that you use all the time, so that should be fine.
  (unless (fboundp 'save-current-buffer)
    (defmacro save-current-buffer (&rest forms)
      `(let ((__scb_current (current-buffer)))
	 (unwind-protect
	     (progn ,@forms)
	   (set-buffer __scb_current)))))
  (unless (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest forms)
      `(save-current-buffer (set-buffer ,buffer) ,@forms)))
  (unless (fboundp 'with-temp-buffer)
    (defmacro with-temp-buffer (&rest forms)
      (let ((temp-buffer (gensym "tb-")))
	`(let ((,temp-buffer
		(get-buffer-create (generate-new-buffer-name " *temp*"))))
	   (unwind-protect
	       (with-current-buffer ,temp-buffer
		 ,@forms)
	     (and (buffer-live-p ,temp-buffer)
		  (kill-buffer ,temp-buffer))))))))

(defvar htmlize-x-library-search-path
  '("/usr/X11R6/lib/X11/"
    "/usr/X11R5/lib/X11/"
    "/usr/lib/X11R6/X11/"
    "/usr/lib/X11R5/X11/"
    "/usr/local/X11R6/lib/X11/"
    "/usr/local/X11R5/lib/X11/"
    "/usr/local/lib/X11R6/X11/"
    "/usr/local/lib/X11R5/X11/"
    "/usr/X11/lib/X11/"
    "/usr/lib/X11/"
    "/usr/local/lib/X11/"
    "/usr/X386/lib/X11/"
    "/usr/x386/lib/X11/"
    "/usr/XFree86/lib/X11/"
    "/usr/unsupported/lib/X11/"
    "/usr/athena/lib/X11/"
    "/usr/local/x11r5/lib/X11/"
    "/usr/lpp/Xamples/lib/X11/"
    "/usr/openwin/lib/X11/"
    "/usr/openwin/share/lib/X11/"))

(defun htmlize-get-color-rgb-hash (&optional rgb-file)
  "Return a hash table mapping X color names to RGB values.
The keys to the hash table are X color names as strings, and the
values are the #rrggbb RGB specifications, extracted from `rgb.txt'.

If RGB-FILE is nil, the function will try hard to find a suitable file
in the system directories.

If no rgb.txt file is found, return nil."
  (let ((rgb-file (or rgb-file (htmlize-locate-file
				"rgb.txt"
				htmlize-x-library-search-path)))
	(hash nil))
    (when rgb-file
      (with-temp-buffer
	(insert-file-contents rgb-file)
	(setq hash (make-hash-table :test 'equal))
	(while (not (eobp))
	  (cond ((looking-at "^\\s-*\\([!#]\\|$\\)")
		 ;; Skip comments and empty lines.
		 )
		((looking-at
		  "[ \t]*\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)")
		 (setf (gethash (downcase (match-string 4)) hash)
		       (format "#%02x%02x%02x"
			       (string-to-number (match-string 1))
			       (string-to-number (match-string 2))
			       (string-to-number (match-string 3)))))
		(t
		 (error
		  "Unrecognized line in rgb.txt: %s"
		  (buffer-substring (point) (progn (end-of-line) (point))))))
	  (forward-line 1))))
    hash))

;; Compile the RGB map when loaded.  On systems where rgb.txt is
;; missing, the value of the variable will be nil, and rgb.txt will
;; not be used.
(defvar htmlize-color-rgb-hash (htmlize-get-color-rgb-hash))

;;; Face handling

(defun htmlize-face-specifies-property (face prop)
  ;; Return t if face specifies PROP, as opposed to it being inherited
  ;; from the default face.  The problem with e.g.
  ;; `face-foreground-instance' is that it returns an instance for
  ;; EVERY face because every face inherits from the default face.
  ;; However, we'd like htmlize-face-{fore,back}ground to return nil
  ;; when called with a face that doesn't specify its own foreground
  ;; or background.
  (if (eq face 'default)
      t
    (let ((spec-list (specifier-spec-list (face-property face prop))))
      (not (null (assq 'global spec-list))))))

(defun htmlize-face-color-internal (face fg)
  ;; Used only under GNU Emacs.  Return the color of FACE, but don't
  ;; return "unspecified-fg" or "unspecified-bg".  If the face is
  ;; `default' and the color is unspecified, look up the color in
  ;; frame parameters.
  (let ((color (if fg (face-foreground face) (face-background face))))
    (when (and (eq face 'default) (null color))
      (setq color (cdr (assq (if fg 'foreground-color 'background-color)
			     (frame-parameters)))))
    (when (or (equal color "unspecified-fg")
	      (equal color "unspecified-bg"))
      (setq color nil))
    (when (and (eq face 'default)
	       (null color))
      ;; Assuming black on white doesn't seem right, but I can't think
      ;; of anything better to do.
      (setq color (if fg "black" "white")))
    color))

(defun htmlize-face-foreground (face)
  ;; Return the foreground color of the face as a string, either a
  ;; color name or #rrggbb.  If FACE does not specify a foreground
  ;; color, return nil.
  (cond (htmlize-running-xemacs
	 ;; XEmacs.
	 (and (htmlize-face-specifies-property face 'foreground)
	      (color-instance-name (face-foreground-instance face))))
	(t
	 ;; GNU Emacs.
	 (htmlize-face-color-internal face t))))

(defun htmlize-face-background (face)
  ;; Return the background color of the face as a string, either a
  ;; color name or #rrggbb.  If FACE does not specify a foreground
  ;; color, return nil.
  (cond (htmlize-running-xemacs
	 ;; XEmacs.
	 (and (htmlize-face-specifies-property face 'background)
	      (color-instance-name (face-background-instance face))))
	(t
	 ;; GNU Emacs.
	 (htmlize-face-color-internal face nil))))

;; Convert COLOR to the #RRGGBB string.  If COLOR is already in that
;; format, it's left unchanged.

(defun htmlize-color-to-rgb-string (color)
  (let (rgb-string)
    (cond ((string-match "^#" color)
	   ;; The color is alredy in #rrggbb format.
	   (setq rgb-string color))
	  ((and htmlize-use-rgb-txt
		htmlize-color-rgb-hash)
	   ;; Use of rgb.txt is requested, and it's available on the
	   ;; system.  Use it.
	   (setq rgb-string (gethash (downcase color) htmlize-color-rgb-hash)))
	  (t
	   ;; We're getting the RGB components from Emacs.
	   (let ((rgb
		  ;; Here I cannot conditionalize on (fboundp ...) 
		  ;; because ps-print under some versions of GNU Emacs
		  ;; defines its own dummy version of
		  ;; color-instance-rgb-components.
		  (if htmlize-running-xemacs
		      (mapcar (lambda (arg)
				(/ arg 256))
			      (color-instance-rgb-components
			       (make-color-instance color)))
		    (mapcar (lambda (arg)
			      (/ arg 256))
			    (x-color-values color)))))
	     (when rgb
	       (setq rgb-string (apply #'format "#%02x%02x%02x" rgb))))))
    ;; If RGB-STRING is still null, it means the color cannot be
    ;; found, for whatever reason.  In that case just punt and return
    ;; COLOR.  Most browsers support a decent set of color names
    ;; anyway.
    (or rgb-string color)))

;; Return FACE's foreground or background as an RGB string.  If the
;; face doesn't specify color, return nil.

(defun htmlize-face-rgb-string (face &optional bg-p)
  (let ((color-name (if bg-p
			(htmlize-face-background face)
		      (htmlize-face-foreground face))))
    (and color-name
	 (htmlize-color-to-rgb-string color-name))))

;; We abstract the face properties we care about into an
;; `htmlize-face' structure.  That way we only have to analyze face
;; properties, which can be time consuming, once per each face.  The
;; mapping between Emacs faces and htmlize-faces is established by
;; htmlize-make-face-hash.

(defstruct htmlize-face
  rgb-foreground			; foreground color, #rrggbb
  rgb-background			; background color, #rrggbb
  boldp					; whether face is bold
  italicp				; whether face is italic
  underlinep				; whether face is underlined
  overlinep				; whether face is overlined
  strikep				; whether face is striked through
  css-name				; CSS name of face
  )

(defun htmlize-emacs-face-to-htmlize-face (face)
  "Convert Emacs face FACE to htmlize-face."
  (let ((object (make-htmlize-face
		 :rgb-foreground (htmlize-face-rgb-string face)
		 :rgb-background (htmlize-face-rgb-string face t))))
    (cond (htmlize-running-xemacs
	   ;; XEmacs doesn't provide a way to detect whether a face is
	   ;; bold or italic, so we need to examine the font instance.
	   ;; #### This probably doesn't work under MS Windows and/or
	   ;; GTK devices.  I'll need help with those.
	   (let* ((font-instance (face-font-instance face))
		  (props (font-instance-properties font-instance)))
	     (when (equalp (cdr (assq 'WEIGHT_NAME props)) "bold")
	       (setf (htmlize-face-boldp object) t))
	     (when (or (equalp (cdr (assq 'SLANT props)) "i")
		       (equalp (cdr (assq 'SLANT props)) "o"))
	       (setf (htmlize-face-italicp object) t))
	     (setf (htmlize-face-strikep object)
		   (face-strikethru-p face))
	     (setf (htmlize-face-underlinep object)
		   (face-underline-p face))))
	  ((fboundp 'face-attribute)
	   ;; GNU Emacs 21.
	   (dolist (attr '(:weight :slant :underline :overline :strike-through))
	     (let ((value (face-attribute face attr)))
	       (when (and value (not (eq value 'unspecified)))
		 (htmlize-face-emacs21-attr object attr value)))))
	  (t
	   ;; Older GNU Emacs.  Some of these functions are only
	   ;; available under Emacs 20+, hence the guards.
	   (when (fboundp 'face-bold-p)
	     (setf (htmlize-face-boldp object) (face-bold-p face)))
	   (when (fboundp 'face-italic-p)
	     (setf (htmlize-face-italicp object) (face-italic-p face)))
	   (setf (htmlize-face-underlinep object)
		 (face-underline-p object))))
    ;; Generate the css-name property.  Emacs places no restrictions
    ;; on the names of symbols that represent faces -- any characters
    ;; may be in the name, even ^@.  We try hard to beat the face name
    ;; into shape, both esthetically and according to CSS1 specs.
    (setf (htmlize-face-css-name object)
	  (let ((name (downcase (symbol-name face))))
	    (when (string-match "\\`font-lock-" name)
	      ;; Change font-lock-FOO-face to FOO.
	      (setq name (replace-match "" t t name)))
	    (when (string-match "-face\\'" name)
	      ;; Drop the redundant "-face" suffix.
	      (setq name (replace-match "" t t name)))
	    (while (string-match "[^-a-zA-Z0-9]" name)
	      ;; Drop the non-alphanumerics.
	      (setq name (replace-match "X" t t name)))
	    (when (string-match "^[-0-9]" name)
	      ;; CSS identifiers may not start with a digit.
	      (setq name (concat "X" name)))
	    ;; After these transformations, the face could come
	    ;; out empty.
	    (when (equal name "")
	      (setq name "face"))
	    ;; Apply the prefix.
	    (setq name (concat htmlize-css-name-prefix name))
	    name))
    object))

(defun htmlize-face-emacs21-attr (hface attr value)
  (case attr
    (:foreground
     (setf (htmlize-face-rgb-foreground hface)
	   (htmlize-color-to-rgb-string value)))
    (:background
     (setf (htmlize-face-rgb-background hface)
	   (htmlize-color-to-rgb-string value)))
    (:weight
     (when (string-match (symbol-name value) "bold")
       (setf (htmlize-face-boldp hface) t)))
    (:slant
     (setf (htmlize-face-italicp hface)
	   (or (eq value 'italic) (eq value 'oblique))))
    (:bold
     (setf (htmlize-face-boldp hface) value))
    (:italic
     (setf (htmlize-face-italicp hface) value))
    (:underline
     (setf (htmlize-face-underlinep hface) value))
    (:overline
     (setf (htmlize-face-overlinep hface) value))
    (:strike-through
     (setf (htmlize-face-strikep hface) value))))

(defun htmlize-make-face-hash (faces)
  ;; Return a hash table mapping faces (typically face symbols, but
  ;; under XEmacs possibly also objects returned by find-face) to the
  ;; associated `htmlize-face' objects.  Keys are faces, not strings,
  ;; so `eq' suffices as test condition.
  (let ((face-hash (make-hash-table :test 'eq))
	face-css-names)
    (dolist (face faces)
      (unless (gethash face face-hash)
	;; Convert FACE to our format.
	(let ((face-obj (htmlize-emacs-face-to-htmlize-face face)))
	  (setf (gethash face face-hash) face-obj)
	  (let* ((css-name (htmlize-face-css-name face-obj))
		 (new-name css-name)
		 (i 0))
	    ;; Uniquify the face's css-name by using FACE-1, FACE-2,
	    ;; etc.
	    (while (member new-name face-css-names)
	      (setq new-name (format "%s-%s" css-name (incf i))))
	    (unless (equal new-name css-name)
	      (setf (htmlize-face-css-name face-obj) new-name))
	    (push new-name face-css-names)))))
    face-hash))

(defun htmlize-faces-in-buffer ()
  "Return a list of faces used by the extents in the current buffer."
  (let (faces)
    ;; Testing for (fboundp 'map-extents) doesn't work because W3
    ;; defines `map-extents' under FSF.
    (if (string-match "XEmacs" emacs-version)
	(let (face)
	  (map-extents (lambda (extent ignored)
			 (setq face (extent-face extent)
			       ;; Note: FACE can be a face or a list of faces.
			       faces (if (listp face)
					 (union face faces)
				       (adjoin face faces)))
			 nil)
		       nil nil nil nil nil 'face))
      ;; FSF Emacs code.
      (save-excursion
	(goto-char (point-min))
	(let (face next)
	  (while (not (eobp))
	    (setq face (get-text-property (point) 'face)
		  next (or (next-single-property-change (point) 'face)
			   (point-max)))
	    ;; FACE can be a face or a list of faces.
	    (setq faces (if (listp face)
			    (union face faces)
			  (adjoin face faces)))
	    (goto-char next))
	  ;; Add faces used by buffer overlays.
	  (dolist (overlay (overlays-in (point-min) (point-max)))
	    (setq face (overlay-get overlay 'face))
	    ;; FACE can be a face or a list of faces.
	    (setq faces (if (listp face)
			    (union face faces)
			  (adjoin face faces)))))
	(setq faces (delete-if-not #'facep faces))))
    faces))

;; htmlize-faces-at-point returns the faces in use at point.  The
;; faces are sorted by increasing priority, i.e. the last face takes
;; precedence.
;;
;; Under XEmacs, this returns all the faces in all the extents at
;; point.  Under GNU Emacs, this returns all the faces in the `face'
;; property and all the faces in the overlays at point.

(cond (htmlize-running-xemacs
       (defun htmlize-faces-at-point ()
	 (let (extent list face)
	   (while (setq extent (extent-at (point) nil 'face extent))
	     (setq face (extent-face extent))
	     (setq list (if (listp face)
			    (nconc (reverse face) list)
			  (cons face list))))
	   ;; No need to reverse the list: PUSH has already
	   ;; constructed it in the reverse display order.
	   list)))
      (t
       (defun htmlize-faces-at-point ()
	 (let (all-faces)
	   ;; Faces from text properties.
	   (let* ((face (get-text-property (point) 'face))
		  (list (if (listp face) (copy-list face) (list face))))
	     (setq all-faces (nconc all-faces (nreverse list))))
	   ;; Faces from overlays.
	   (let ((overlays
		  ;; Sort overlays by size, so that more specific
		  ;; overlays set precedence.  The number of overlays
		  ;; at each one position should be very small, so
		  ;; this sort shouldn't slow things down.
		  (sort (overlays-at (point))
			(lambda (o1 o2)
			  (< (- (overlay-end o1) (overlay-start o1))
			     (- (overlay-end o2) (overlay-start o2))))))
		 list face)
	     (dolist (overlay overlays)
	       (setq face (overlay-get overlay 'face))
	       (setq list (if (listp face)
			      (nconc (reverse face) list)
			    (cons face list))))
	     (setq all-faces (nconc all-faces list)))
	   ;; We don't support property lists, such as (:foreground
	   ;; ...).  (Supporting them is hard because they need to be
	   ;; mapped to face-less classes, and those classes must be
	   ;; known in advance.)  For now, only leave faces.
	   (delete-if-not 'facep all-faces)))))

;;; CSS1 support

(defun htmlize-css-doctype ()
  nil					; no doc-string
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\">")

;; Internal function; not a method.
(defun htmlize-css-specs (face)
  (let (result)
    (when (htmlize-face-rgb-foreground face)
      (push (format "color: %s;" (htmlize-face-rgb-foreground face))
	    result))
    (when (htmlize-face-rgb-background face)
      (push (format "background-color: %s;" (htmlize-face-rgb-background face))
	    result))
    (when (htmlize-face-boldp face)
      (push "font-weight: bold;" result))
    (when (htmlize-face-italicp face)
      (push "font-style: italic;" result))
    (when (htmlize-face-underlinep face)
      (push "text-decoration: underline;" result))
    (when (htmlize-face-overlinep face)
      (push "text-decoration: overline;" result))
    (when (htmlize-face-strikep face)
      (push "text-decoration: line-through;" result))
    (nreverse result)))

(defun htmlize-css-insert-head (face-hash)
  (insert "    <style type=\"text/css\">\n    <!--\n")
  (insert "      body {\n        /* default */\n        "
	  (mapconcat #'identity
		     (htmlize-css-specs (gethash 'default face-hash))
		     "\n        ")
	  "\n      }\n")
  (maphash
   (lambda (face face-object)
     (let ((cleaned-up-face-name (symbol-name face)))
       ;; If face name contains `--' or `*/', we must nix them out.
       (while (string-match "--" cleaned-up-face-name)
	 (setq cleaned-up-face-name (replace-match "-" t t
						   cleaned-up-face-name)))
       (while (string-match "\\*/" cleaned-up-face-name)
	 (setq cleaned-up-face-name (replace-match "XX" t t
						   cleaned-up-face-name)))
       (let ((specs (htmlize-css-specs face-object)))
	 (insert "      ." (htmlize-face-css-name face-object))
	 (if (null specs)
	     (insert " {")
	   (insert " {\n        /* " cleaned-up-face-name " */\n        "
		   (mapconcat #'identity specs "\n        ")))
	 (insert "\n      }\n"))))
     face-hash)
  (insert htmlize-hyperlink-style
	  "    -->\n    </style>\n"))

(defun htmlize-css-insert-text (text faces buffer)
  ;; Insert TEXT colored with FACES into BUFFER.
  (dolist (face faces)
    (princ "<span class=\"" buffer)
    (princ (htmlize-face-css-name face) buffer)
    (princ "\">" buffer))
  (princ text buffer)
  (dolist (face faces)
    (ignore face)
    (princ "</span>" buffer)))

;;; <font> support

(defun htmlize-font-doctype ()
  nil					; no doc-string

  ;; According to DTDs published by the W3C, it is illegal to embed
  ;; <font> in <pre>.  This makes sense in general, but is bad for
  ;; htmlize's intended usage of <font> to specify the document color.

  ;; To make generated HTML legal, htmlize.el used to specify the SGML
  ;; declaration of "HTML Pro" DTD here.  HTML Pro aka Silmaril DTD
  ;; was a project whose goal was to produce a DTD that would
  ;; encompass all the incompatible HTML extensions procured by
  ;; Netscape, MSIE, and other players in the field.  Apparently the
  ;; project got abandoned, the last available version being "Draft 0
  ;; Revision 11" from January 1997, as documented at
  ;; <http://validator.w3.org/sgml-lib/pro/html/dtds/htmlpro.html>.

  ;; Since by now (2001) HTML Pro is remembered by none but the most
  ;; die-hard early-web-days nostalgics and used by not even them,
  ;; there is no use in specifying it.  So we return the standard HTML
  ;; 4.0 declaration, which makes generated HTML technically illegal.
  ;; If you have a problem with that, use the `css' generation engine
  ;; which I believe creates fully conformant HTML.

  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\">"

  ;; Now-abandoned HTML Pro declaration.
  ;"<!DOCTYPE HTML PUBLIC \"+//Silmaril//DTD HTML Pro v0r11 19970101//EN\">"
  )

(defun htmlize-font-body-tag (face-hash)
  (let ((face-object (gethash 'default face-hash)))
    (format "<body text=\"%s\" bgcolor=\"%s\">"
	    (htmlize-face-rgb-foreground face-object)
	    (htmlize-face-rgb-background face-object))))

(defun htmlize-font-insert-text (text faces buffer)
  (let (bold italic underline strike fg)
    ;; Merge the faces.
    (dolist (face faces)
      ;; A non-null boolean attribute in any face sets the attribute.
      (and (htmlize-face-boldp face)      (setq bold t))
      (and (htmlize-face-italicp face)    (setq italic t))
      (and (htmlize-face-underlinep face) (setq underline t))
      (and (htmlize-face-strikep face)    (setq strike t))
      ;; The foreground/background of the last face in the list wins.
      (and (htmlize-face-rgb-foreground face)
	   (setq fg (htmlize-face-rgb-foreground face))))

    ;; Print HTML based on the merge.
    (princ (concat
	    (and fg        (format "<font color=\"%s\">" fg))
	    (and bold      "<b>")
	    (and italic    "<i>")
	    (and underline "<u>")
	    (and strike    "<strike>"))
	   buffer)
    ;; Print the text.
    (princ text buffer)
    ;; Close the tags.
    (princ (concat
	    (and strike    "</strike>")
	    (and underline "</u>")
	    (and italic    "</i>")
	    (and bold      "</b>")
	    (and fg        "</font>"))
	   buffer)))

(defun htmlize-despam-address (string)
  "Replace every occurrence of '@' in STRING with &#64;.
`htmlize-make-hyperlinks' uses this to spam-protect mailto links
without modifying their meaning."
  ;; Suggested by Ville Skytta.
  (while (string-match "@" string)
    (setq string (replace-match "&#64;" nil t string)))
  string)

(defun htmlize-make-hyperlinks ()
  "Make hyperlinks in HTML."
  ;; Function originally submitted by Ville Skytta.  Rewritten by
  ;; Hrvoje Niksic, then modified by Ville Skytta and Hrvoje Niksic.
  (goto-char (point-min))
  (while (re-search-forward
	  "&lt;\\(\\(mailto:\\)?\\([-=+_.a-zA-Z0-9]+@[-_.a-zA-Z0-9]+\\)\\)&gt;"
	  nil t)
    (let ((address (match-string 3))
	  (link-text (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "&lt;<a href=\"mailto:"
	      (htmlize-despam-address address)
	      "\">"
	      (htmlize-despam-address link-text)
	      "</a>&gt;")))
  (goto-char (point-min))
  (while (re-search-forward "&lt;\\(\\(URL:\\)?\\([a-zA-Z]+://[^;]+\\)\\)&gt;"
			    nil t)
    (let ((url (match-string 3))
	  (link-text (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "&lt;<a href=\"" url "\">" link-text "</a>&gt;"))))

;; Tests for htmlize-make-hyperlinks:

;; <mailto:hniksic@xemacs.org>
;; <http://fly.srk.fer.hr>
;; <URL:http://www.xemacs.org>
;; <http://www.mail-archive.com/bbdb-info@xemacs.org/>
;; <hniksic@xemacs.org>
;; <xalan-dev-sc.10148567319.hacuhiucknfgmpfnjcpg-john=doe.com@xml.apache.org>

(defmacro htmlize-method (method &rest args)
  (let ((func (gensym "hm-")))
    `(let ((,func (intern (format "htmlize-%s-%s" htmlize-output-type ',method))))
       (and (fboundp ,func)
	    (funcall ,func ,@args)))))

(defun htmlize-buffer-1 ()
  ;; Internal function; don't call it from outside this file.  Htmlize
  ;; current buffer, writing the resulting HTML to a new buffer, and
  ;; return it.  Unlike htmlize-buffer, this doesn't change current
  ;; buffer or use switch-to-buffer.
  (save-excursion
    ;; Protect against the hook changing the current buffer.
    (save-excursion
      (run-hooks 'htmlize-before-hook))
    (let ((face-hash (htmlize-make-face-hash
		      (adjoin 'default (htmlize-faces-in-buffer))))
	  ;; Generate the new buffer.  It's important that it inherits
	  ;; default-directory from the current buffer.
	  (htmlbuf (generate-new-buffer (if (buffer-file-name)
					    (htmlize-make-file-name
					     (file-name-nondirectory
					      (buffer-file-name)))
					  "*html*")))
	  (title (buffer-name (current-buffer)))
	  next-change text faces face-objects)
      ;; Initialize HTMLBUF and insert the HTML prolog.
      (with-current-buffer htmlbuf
	(buffer-disable-undo)
	(insert (htmlize-method doctype) ?\n
		(format "<!-- Created by htmlize-%s in %s mode. -->\n"
			htmlize-version htmlize-output-type)
		"<html>\n  <head>\n"
		"    <title>" (htmlize-protect-string title) "</title>\n"
		(if htmlize-html-charset
		    (format (concat "    <meta http-equiv=\"Content-Type\" "
				    "content=\"text/html; charset=%s\">\n")
			    htmlize-html-charset)
		  "")
		htmlize-head-tags)
	(htmlize-method insert-head face-hash)
	(insert "  </head>"
		"\n  "
		(or (htmlize-method body-tag face-hash)
		    "<body>")
		"\n    <pre>\n"))
      ;; This loop traverses and reads the source buffer, appending
      ;; the resulting HTML to HTMLBUF with `princ'.  This method is
      ;; fast because: 1) it doesn't require examining the text
      ;; properties char by char (htmlize-next-change is used to move
      ;; between runs with the same face), and 2) it doesn't require
      ;; buffer switches, which are slow in Emacs.
      (goto-char (point-min))
      (while (not (eobp))
	;; Using get-char-property instead of get-text-property
	;; insures that all the extents are examined, not only the
	;; ones that belong to text properties.  Likewise for
	;; `htmlize-next-change'.
	(setq faces (htmlize-faces-at-point)
	      next-change (htmlize-next-change (point) 'face))
	;; Convert faces to face objects.
	(setq face-objects (mapcar (lambda (f) (gethash f face-hash)) faces))
	;; Extract buffer text, sans the invisible parts.  Then
	;; untabify it and escape the HTML metacharacters.
	(setq text (htmlize-buffer-substring (point) next-change))
	(when (string-match "\t" text)
	  (setq text (htmlize-untabify text (current-column))))
	(setq text (htmlize-protect-string text))
	;; Don't bother writing anything if there's no text (this
	;; happens in invisible regions).
	(when (> (length text) 0)
	  ;; Insert the text, with HTML annotation around it.
	  (htmlize-method insert-text text face-objects htmlbuf))
	(goto-char next-change))

      ;; Insert the epilog.
      (with-current-buffer htmlbuf
	(insert "</pre>\n  </body>\n</html>\n")
	(when htmlize-generate-hyperlinks
	  (htmlize-make-hyperlinks))
	(goto-char (point-min))
	(when htmlize-html-major-mode
	  ;; What sucks about this is that the minor modes, most notably
	  ;; font-lock-mode, won't be initialized.  Oh well.
	  (funcall htmlize-html-major-mode))
	(run-hooks 'htmlize-after-hook)
	(buffer-enable-undo))
      htmlbuf)))

;;;###autoload
(defun htmlize-buffer (&optional buffer)
  "Convert buffer to HTML, preserving the font-lock colorization.
The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the
current window."
  (interactive)
  (let ((htmlbuf (with-current-buffer (or buffer (current-buffer))
		   (htmlize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer htmlbuf))
    htmlbuf))

;;;###autoload
(defun htmlize-region (beg end)
  "Convert the region to HTML, preserving the font-lock colorization.
The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the
current window."
  (interactive "r")
  ;; We don't want the region highlighting to get in the way.
  (when (fboundp 'zmacs-deactivate-region)
    (zmacs-deactivate-region))
  (let ((htmlbuf (save-restriction
		   (narrow-to-region beg end)
		   (htmlize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer htmlbuf))
    htmlbuf))

(defun htmlize-make-file-name (file)
  "Make an HTML file name from FILE.
The HTML file name is the regular file name, with its extension
changed to `.html'.  The exception are the file names which don't
have an extension, or those which are already `.html' -- in these
cases, \".html\" is simply appended.

Some examples:

 (htmlize-make-file-name \"foo.c\")     ==> \"foo.html\"
 (htmlize-make-file-name \"foo.b.c\")   ==> \"foo.b.html\"
 (htmlize-make-file-name \"foo\")       ==> \"foo.html\"
 (htmlize-make-file-name \"foo.html\")  ==> \"foo.html.html\"
 (htmlize-make-file-name \".emacs\")    ==> \".emacs.html\""
  (let ((extension (htmlize-file-name-extension file))
	(sans-extension (file-name-sans-extension file)))
    (if (or (equal extension "html")
	    (equal extension "htm")
	    (equal sans-extension ""))
	(concat file ".html")
      (concat sans-extension ".html"))))

;;;###autoload
(defun htmlize-file (file &optional target)
  "Find FILE, fontify it convert it to HTML, and save the result.

This function does not modify current buffer or point.  If FILE is
already being visited in a buffer, the contents of that buffer are
used for HTML-ization.  Otherwise, FILE is read into a temporary
buffer, which is disposed of after use.  FILE's buffer is explicitly
fontified before HTML-ization.  If a form of highlighting other than
font-lock is desired, please use `htmlize-buffer' directly.

The function `htmlize-make-file-name', is used to determine the name
of the resulting HTML file.  In normal cases, the FILE's extension is
replaced with `html', e.g. \"foo.c\" becomes \"foo.html\".  See the
documentation of `htmlize-make-file-name' for more details.

If TARGET is specified and names a directory, the resulting file will
be saved there instead of to FILE's directory.  If TARGET is specified
and does not name a directory, it will be used as output file name."
  (interactive (list (read-file-name
		      "HTML-ize file: "
		      nil nil nil (and (buffer-file-name)
				       (file-name-nondirectory
					(buffer-file-name))))))
  (let* ((was-visited (get-file-buffer file))
	 ;; Prevent `find-file-noselect' from triggering font-lock.
	 ;; We'll fontify manually below.  Set these to nil to prevent
	 ;; double fontification; we'll fontify manually below.
	 (font-lock-auto-fontify nil)
	 (global-font-lock-mode nil)
	 ;; Determine the output file name.
	 (output-file (if (and target (not (file-directory-p target)))
			  target
			(expand-file-name
			 (htmlize-make-file-name (file-name-nondirectory file))
			 (or target (file-name-directory file))))))
    ;; Find FILE, fontify it, HTML-ize it, and write it to FILE.html.
    ;; The `unwind-protect' forms are used to make certain the
    ;; temporary buffers go away in case of unexpected errors or C-g.
    (with-current-buffer (find-file-noselect file t)
      (unwind-protect
	  (progn
	    (font-lock-fontify-buffer)
	    (with-current-buffer (htmlize-buffer-1)
	      (unwind-protect
		  (progn
		    (run-hooks 'htmlize-file-hook)
		    (write-region (point-min) (point-max) output-file))
		(kill-buffer (current-buffer)))))
	;; If FILE was not previously visited, its buffer is temporary
	;; and must be killed.
	(unless was-visited
	  (kill-buffer (current-buffer)))))))

;;;###autoload
(defun htmlize-many-files (files &optional target-directory)
  "HTML-ize files specified by FILES, and save them to `.html' files.
If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file."
  (interactive
   (list
    (let (list file)
      ;; Use empty string as DEFAULT because setting DEFAULT to nil
      ;; defaults to the directory name, which is not what we want.
      (while (not (equal (setq file (read-file-name
				     "HTML-ize file (RET to finish): "
				     (and list (file-name-directory
						(car list)))
				     "" t))
			 ""))
	(push file list))
      (nreverse list))))
  (dolist (file files)
    (htmlize-file file target-directory)))

;;;###autoload
(defun htmlize-many-files-dired (arg &optional target-directory)
  "HTMLize dired-marked files."
  (interactive "P")
  (htmlize-many-files (dired-get-marked-files nil arg) target-directory))

(provide 'htmlize)

;;; htmlize.el ends here
