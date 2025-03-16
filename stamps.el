;;; stamps.el --- Annotation and note-taking tool.  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mirko Hernandez

;; Author: Mirko Hernandez <mirkoh@fastmail.com>
;; Maintainer: Mirko Hernandez <mirkoh@fastmail.com>>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Keywords: annotations notes
;; URL: https://github.com/MirkoHernandez/stamps
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Stamps is an annotator tool that can also be used for note-taking.

;;; Code
;;;; Structures
;; NOTE: All the  notes are stored in  a stamps container. The  notes do
;; not indicate the file or url they refer.
(cl-defstruct (stamps-note
	       (:constructor stamps-make-note
			     (&key type
				   locator
				   precise-locator
				   file
				   line
				   column
				   source)))
  "TYPE  is the  type  of file  the note  references; LOCATOR and  PRECISE-
LOCATOR indicate the position that  the note references, page and
coordinates  for a  document,  file (or  url)  and timestamp  for
media; FILE, LINE,  COLUMN describe the note  location; SOURCE is
usually an xref struct used  for navigating to the note."
  type  locator  precise-locator file line column  source)

(cl-defstruct (stamps-container
	       (:constructor stamps-make-container (&key
						    citekey
						    regexp
						    type
						    notes)))
  "NOTES is a  hash table of notes, each key  is a number describing
the note index, the value  is the note object; NUMBER-OF-NOTES is
the number of notes; RESOURCES is a list of files or urls related
to  the citekey;  ACTIVE-RESOURCE is  the last  visited resource;
ACTIVE-NOTE is the index (the key) of the last visited note."
  citekey
  regexp
  (notes nil)
  (number-of-notes 0)
  (resources)
  (active-resource)
  (active-note 0)
  type)

(cl-defstruct (stamps-table
	       (:constructor stamps-make-tables (&key
						     files
						     urls
						     loaded-resources
						     note-files
						     containers)))
  ""
  (files (make-hash-table :test 'equal))
  (urls (make-hash-table :test 'equal))
  (loaded-resources (make-hash-table :size 512 :test 'equal))
  (note-files (make-hash-table :size 512 :test 'equal))
  (containers (make-hash-table  :test 'equal)))

;;;; Table Methods
(cl-defmethod stamps-container-file-notes ((container stamps-container)
					       file)
  "Return a list of notes that correspond with FILE."
  (pcase-let (((cl-struct stamps-container notes) container))))

(defun stamps-filter-notes-by-file (container file)
  (let* ((notes (hash-table-values (stamps-container-notes container)))
	 (file (expand-file-name  file)))
    (-filter (lambda (n)
	       (equal file (stamps-note-file n))
	       ) notes)))

(cl-defmethod stamps-container-add-note ((container stamps-container)
					 index note)
  (pcase-let (((cl-struct stamps-container notes number-of-notes) container))
    (aset notes index note)
    (setf (stamps-container-number-of-notes container)
	  (length notes))))

;;;; Regexps
;; Timestamps
(defvar stamps-timestamp-regexp  ".*\\(\\<[012]?[0-9]\\(\\(:\\([0-5][0-9]\\)\\)\\(:\\([0-5][0-9]?\\)\\)\\)\\>\\)"
  "Regex used to capture a timestamp location. First group is used to capture the timestamp")

(defvar stamps-timestamp-format-string
  "[[cite:@%s %s][%s]]  "
  "")

(defvar stamps-quote-regexp
  (concat
   ;; (1) indentation
   "^\\([ \t]*\\)#\\+BEGIN_QUOTE[ \t]*"
   ;; (2) body
   "\\(\\(?:.\\|\n\\)*?\n\\)??[ \t]*#\\+END_QUOTE"))

(defvar stamps-timestamp-stamp-regexp
  (rx (seq
       (regexp "\\[\\([^[]+\\) +\\(.*\\)\\]")
       (regexp stamps-timestamp-regexp))))

;; Citations
(defconst stamps-page-regexp
  " +\\(p\\|p\.\\|pp\.\\) +\\([[:digit:]-]+\\)"
  "Regex used to capture a page number. Second group is used to capture the number.")

(defconst stamps-citation-string
  "[cite:@%s p. %s]"
  "Format  string  used to  create  the  citation  string.
It  handles  the  citekey and page  number.")

(defconst stamps-citation-string-with-metadata
  "[cite:@%s p. %s] [[%s][%s]]"
  "Format  string  used to  create  the  citation  string.
It  handles  the  citekey,   page  number,  locator  (coordinates),  and
description.")


;;;; Global Variables
(defvar stamps-active-container nil
  "")

(defvar stamps-directory nil
  "Directory where notes are searched.")

(defvar stamps-active-citekey nil
  "")

(defvar stamps-default-locator-string "loc"
  "")

(defvar stamps-pdf-annotation-function
  'stamps-insert-pdf-annotation)

;;;; Tables
(defvar stamps-tables
      (stamps-make-tables))

;;;; Helpers - Find Notes
(defun stamps-container-files (container)
  (mapcar 'stamps-note-file
	  (hash-table-values
	   (stamps-container-notes container))))

(defun stamps-create-find-citekey-regexp (citekey)
  (rx  (or (regexp (format "%s +\\(timestamp\\|p\\|p\.\\|pp\.\\)+" citekey))
	   (regexp (format "\\[\\[%s.*\\]\\[.*\\]\\]" citekey)))))

(defun stamps-media-locator-regexp (citekey)
  (rx  (regexp (format "%s +\\([^]]\\)]" citekey))))

(defun stamps-reference-at-point (&optional context)
  "Return citation-reference org-element at point."
  (interactive)
  (when-let ((context (or context (org-element-context))))
    (when (eq 'citation-reference (org-element-type context))
      context)))

(defun stamps-suffix-at-point ()
  (interactive)
  "Return the suffix string associated with a citation org-element at point."
  (when-let* ((reference (stamps-reference-at-point (org-element-context)))
	      (suffix (car (org-element-property :suffix reference))))
    (when suffix
      (message "%s"
	       (substring-no-properties suffix)))))

(defun stamps-get-table (table)
  (cl-struct-slot-value 'stamps-table table stamps-tables))

(defun stamps-get-table-keys (table)
  (hash-table-keys
   (cl-struct-slot-value 'stamps-table table stamps-tables)))

(defun stamps-get-from-table (key table)
  (gethash key
	  (stamps-get-table table)))

(defun stamps-put-in-table (key value table)
  (puthash key value
	   (stamps-get-table table)))

(defun stamps-set-table (table value)
  (setf (slot-value stamps-tables table) value))

;;;; Find Notes
(defun stamps-get-loaded-citekey (&optional resource)
  (let* ((resource (or resource (buffer-file-name)))
	 (filename (and (file-regular-p resource)
			(file-name-nondirectory resource)))
	 (abs-filename (and (file-regular-p resource)
			    (expand-file-name resource)))
	 (is-url (unless (or filename abs-filename)))
	 (citekey (or (stamps-get-from-table filename 'loaded-resources)
		      (stamps-get-from-table abs-filename 'loaded-resources)
		      (stamps-get-from-table resource 'loaded-resources))))
    (cl-values citekey is-url filename abs-filename)))

(defun stamps-get-citekey (&optional resource)
  "Get the citekey associated with RESOURCE. Or try to get one using the
current buffer's file name."

  (cl-multiple-value-bind (citekey is-url filename abs-filename)
      (stamps-get-loaded-citekey resource)
    (if citekey
	citekey
      (stamps-load-resources)
      (let* (;; files or urls.
	     (resources (if is-url
			    (stamps-get-table 'urls)
			  (stamps-get-table 'files)))
	     result)
	(maphash (lambda (key value)
		   (when (or (member filename value)
			     (member abs-filename value)
			     (member resource value))
		     (setq result key)))
		 resources)
	(if result
	    result
	  (message "No citekey found for %s." filename)
	  nil)))))

(defun stamps-get-citekeys-with-resource ()
  (stamps-load-resources)
  (append
   (hash-table-keys (stamps-get-table 'files))
   (hash-table-keys (stamps-get-table 'urls))))

(defun stamps-get-resources-from-citekey (citekey)
  (when citekey
    (or (stamps-get-from-table citekey 'files)
	(stamps-get-from-table citekey 'urls)
	(progn
	  (stamps-load-resources)
	  (or (stamps-get-from-table citekey 'files)
	      (stamps-get-from-table citekey 'urls))))))

(defun stamps-fetch-citekey-sources (citekey &optional dir)
  "Search `stamps-directory' for files that match a CITEKEY based regexp."
  (let* ((citekey-regexp (stamps-create-find-citekey-regexp citekey))
	 (matches  (xref-matches-in-directory
		    citekey-regexp
		    "*"
		    (or dir stamps-directory)
		    nil)))
    matches))

(defun stamps-fetch-sources (regexp &optional dir)
  (let* ((matches  (xref-matches-in-directory
		    regexp
		    "*"
		    (or dir stamps-directory)
		    nil)))
    matches))

(defun stamps-fetch-in-container (container regex)
  (xref-matches-in-files
   regex
   (delete-dups
    (stamps-container-files container))))

;;;; Helpers - Get note data.
(defun stamps-load-resources ()
  (message "Loading resources" )
  (let ((inhibit-message t)) ;; omit missing files message.
    (stamps-set-table 'urls (citar-get-links))
    (stamps-set-table 'files (citar-get-files))))

(defun stamps-extract-page (str)
  "Extract page number from STR."
  (when (and str (string-match stamps-page-regexp str)) ; if there is a range of pages capture the first one.
    (let ((match (match-string 2 str)))
      (when match
	(substring-no-properties
	 (car (string-split match "-")))))))

(defun stamps-extract-media-locator (str)
  (when (and str (string-match stamps-media-locator-regexp str)) ; if there is a range of pages capture the first one.
    (let ((match (match-string 2 str)))
      (when match
	(substring-no-properties
	 (car (string-split match "-")))))))

(defun stamps-extract-citekey (str)
  (when (and str (string-match "cite:@\\([[:graph:]]+\\)[ ;]" str))
    (let ((match (match-string 1 str)))
      (substring-no-properties
       match))))

(defun stamps-extract-locator (str)
  "Extract note precise locator from STR."
  (cond ((and str (string-match "\\(([0-9. ]*)\\)" str))
	 (let ((match (match-string 1 str)))
	   (when match
	     (substring-no-properties
	      (car (string-split match "-"))))))))

(defun stamps-extract-timestamp (str)
  "Extract note timestamp from STR."
  (cond ((and str (string-match stamps-timestamp-regexp str))
	 (let ((match (match-string 1 str)))
	   (when match
	     (substring-no-properties
	      match))))))

(defun stamps-extract-metadata (str)
  "Extract tag from STR."
  (cond ((and str (string-match stamps-metadata-regexp str))
	 (let ((match (match-string 2 str)))
	   (when match
	     (substring-no-properties
	      match))))))

(defun stamps-extract-from-summary (summary)
  (let* ((page (stamps-extract-page summary))
	 (timestamp (stamps-extract-timestamp summary))
	 (type (cond (timestamp 'media)
		     (page 'document)))
	 (precise-locator-str (stamps-extract-locator summary))
	 (precise-locator (and precise-locator-str page (read precise-locator-str))))
    (cl-values page timestamp precise-locator type)))

;;;; Get notes
(defun stamps-source-to-note (source)
  "Process SOURCE to create note object."
  (when (xref-item-p source)
    (let* ((summary (xref-item-summary source))
	   (file (xref-file-location-file (xref-item-location source)))
	   (line (xref-file-location-line (xref-item-location source))))
      (cl-multiple-value-bind (page timestamp precise-locator type)
	  (stamps-extract-from-summary summary)
	(cond
	 (timestamp
	  (stamps-make-note :type 'media
			    :locator page
			    :precise-locator timestamp
			    :file file
			    :line line
			    :source source))
	 (page
	  (stamps-make-note  :type 'document
			     :locator page
			     :precise-locator precise-locator
			     :file file
			     :line line
			     :source source))
	 (t
	  (stamps-make-note  :type 'regexp
			     :file file
			     :line line
			     :source source)))))))

(defun stamps-load-container (container sources &optional resources)
  "RESOURCES are files or urls."
  (let ((index 0)
	(citekey (stamps-container-citekey container))
	(note-vector (make-vector (length sources) nil))
	type)
    (setf (stamps-container-notes container) note-vector)
    (while-let ((note (stamps-source-to-note (pop sources)))
		(file (stamps-note-file note)))
      (stamps-container-add-note container index note)
      (unless type
	(setq type (stamps-note-type note)))
      (if-let (value (stamps-get-from-table file 'files))
	  (stamps-put-in-table file (cl-union value (list citekey)) 'files)
	(stamps-put-in-table file (list citekey) 'files))
      (cl-incf index))
    ;; Set container type
    (setf (stamps-container-type container) type)
    ;; Sort
    (stamps-sort-container-notes container)
   ;; Add resources
    (when resources
      (setf (stamps-container-resources container) resources)
      (while-let ((resource (pop resources)))
	(stamps-put-in-table resource citekey 'loaded-resources)))))

(defun stamps-load-citekey (citekey)
  "Create   container  and   find  notes   related  to   CITEKEY  in
`stamps-directory' .Return the container associated with citekey."
  (when citekey
    (stamps-load-resources)
    (let* ((sources (stamps-fetch-citekey-sources citekey))
	   (container (stamps-make-container :citekey citekey))
	   (resources (stamps-get-resources-from-citekey citekey )))
      (stamps-load-container container sources resources)
      (message "%s notes loaded for %s"
	       (stamps-container-number-of-notes container)
	       citekey)
      (stamps-put-in-table citekey container 'containers))))

(defun stamps-load-regexp ()
  (interactive)
  (let* ((regexp (read-string "Regexp:"))
	 (sources (stamps-fetch-sources regexp))
	 (container  (when (not (string-empty-p regexp))
		       (stamps-make-container :regexp regexp))))

    (stamps-load-container container sources)
    (setq stamps-active-container
	  container)))

(defun stamps-load (&optional resource)
  (interactive)
  (let* ((filename (or (and resource (expand-file-name resource)) (buffer-file-name)))
	 (citekey-from-current-file (and filename  (stamps-get-citekey filename)))
	 (citekey (or citekey-from-current-file

		      (completing-read "citekey:" (stamps-get-citekeys-with-resource) ))))
    (when citekey
      (setq stamps-active-container
	    (stamps-load-citekey citekey)))))



(defun stamps-get-container (&optional citekey)
  ""
  (interactive)
  (let* ((citekey (or citekey
		      (stamps-get-citekey (buffer-file-name)))))
    (or (stamps-get-from-table citekey 'containers)
	(stamps-load-citekey citekey))))

;;; Filter Notes
(defun stamps-filter-by-directory ()
  ""
  (interactive)
  (when-let* ((container stamps-active-container)
	      (notes (stamps-container-notes container))
	      (directories (seq-map (lambda (n)
				      (file-name-directory
				       (stamps-note-file n))
				      ) notes))
	      (directories-non-dups (delete-dups dos))
	      (selected-dir (completing-read "Directory:" directories-non-dups))
	      (filtered-notes (seq-filter (lambda (n)
					    (string-match-p selected-dir (stamps-note-file n))
					    ) notes)))
    (setf (stamps-container-notes container) filtered-notes)
    (setf (stamps-container-number-of-notes container) (length filtered-notes))))

;;;; Helpers - Create Notes
(defun stamps-get-pdf-window ()
  (some-window (lambda (w)
		 (with-selected-window w
		   (equal major-mode 'pdf-view-mode)))))

(defun stamps-get-other-window ()
  (when (one-window-p)
    (split-window-right))
  (other-window-for-scrolling))

(defun stamps-get-note-window ()
  (some-window (lambda (w)
		 (with-selected-window w
		   (and
		    (not buffer-read-only)
		    (not (equal major-mode 'pdf-view-mode)))))))

;;;; Creating PDF notes
;; NOTE: Creating notes for a PDF  document involves checking if the current
;; document is part  of the bibliography, then using  the current page
;; and coordinates to create a citation string.
(defun stamps-pdf-page-data ()
  (when-let ((w (stamps-get-pdf-window)))
    (with-selected-window w
      (let ((page (number-to-string (pdf-view-current-page))) ; page is the locator
	    (coords (when  (pdf-view-active-region-p) ; coords is the precise locator.
		      (pdf-view-active-region)))
	    (citekey (stamps-get-citekey)))
	(cl-values citekey page coords)))))

(defun stamps-insert-pdf-annotation (citekey page coords)
  (when-let ((w (stamps-get-note-window)))
    (with-selected-window w
      ;; If  cursor is  placed in  a word  insert a space.
      (unless
	  (or
	   (eq ?  (char-before))
	   (eq ?\n  (char-before))
	   (eq ?        (char-before)))
	(search-forward-regexp  "\\_>" nil t )
	(insert " "))
      (if coords
	  (insert (format stamps-citation-string-with-metadata citekey page coords stamps-default-locator-string))
	(insert (format stamps-citation-string citekey page))))))

(defun stamps-annotate-pdf ()
  (interactive)
  (cl-multiple-value-bind (citekey page coords)
      (stamps-pdf-page-data)
    (funcall stamps-pdf-annotation-function citekey page coords)))

(defun stamps-get-media-path ()
  (require 'mpv)
  (mpv-get-property "path"))

(defun stamps-sort-container-notes (container)
  (let ((type (stamps-container-type container))
	(notes (stamps-container-notes container)))
    (pcase type
      ('document
       (setf (stamps-container-notes container)
	     (sort notes 'stamps-sort-notes-by-page)))
      ('media
       (setf (stamps-container-notes container)
	     (sort notes 'stamps-sort-notes-by-timestamp))))))

(defun stamps-sort-notes-by-page (a b)
  "Sort notes by page and coordinante position"
  (let ((page-a (string-to-number (stamps-note-locator a)))
	(page-b (string-to-number (stamps-note-locator b))))

    (if (= page-a page-b)
	(let ((coord-a (stamps-note-precise-locator a))
	      (coord-b (stamps-note-precise-locator b)))
	  (cond ((and (listp coord-a) (listp coord-b)
		      (listp (car coord-a))
		      (listp (car coord-b))
		      (numberp (caar coord-a))
		      (numberp (caar coord-b)))
		 ;; both pages have coords.
		 (< (caar coord-a) (caar coord-b)))
		(coord-a
		 nil)
		(coord-b
		 t)
		(t t)))
      (<  page-a page-b))))

(defun stamps-sort-notes-by-timestamp (a b)
  "Sort notes by timestamp."
  (let ((timestamp-a (stamps-note-precise-locator a))
	(timestamp-b (stamps-note-precise-locator b)))
    (string< timestamp-a timestamp-b)))

;;;; Helpers -- goto notes
(defun stamps-display-pdf-page (file page coords)
  (when file
    (find-file file))
  (when (and page (equal major-mode 'pdf-view-mode))
    (pdf-view-goto-page (string-to-number page))
    (when coords
      (pdf-view-deactivate-region)
      (setq pdf-view-active-region
	    (read coords))
      (when (pdf-view-active-region-p)
	(pdf-view-display-region)))))

;;;; Goto notes
(defun stamps-open-note()
  (interactive)
  (let* ((citekey  (completing-read "citekey:" (stamps-get-citekeys-with-resource)))
	 (container (stamps-get-container citekey))
	 (notes (stamps-container-notes container))
	 (locators (mapcar (lambda (n)
			     (let* ((path (stamps-note-file n))
				    (file  (file-name-nondirectory path))
				    (line (number-to-string (stamps-note-line n))))
			       `(,(concat file "  :: " line) ,path)))
			   ;; (hash-table-values
			    ;; notes)
			   notes))
	 (completion-extra-properties
	  (list :annotation-function
		(lambda (n)
		  (format "%10s" (car (alist-get n locators nil nil 'string=))))))
	 (note (cl-second (assoc (completing-read "note:" locators) locators))))
    (find-file note)))

(defun stamps-open-citekey-note()
  (interactive)
  (let* ((citekey  (completing-read "citekey:" (stamps-get-citekeys-with-resource)))
	 (container (stamps-get-container citekey))
	 (notes (stamps-container-notes container))
	 (locators (mapcar 'stamps-note-source (hash-table-values notes)) ))
    (xref-show-xrefs locators nil)))

(defun stamps-open-container-note()
  (interactive)
  (let* ((citekey  (completing-read "citekey:" (stamps-get-citekeys-with-resource)))
	 (container (stamps-get-container citekey))
	 (notes (stamps-container-notes container))
	 (locators (mapcar 'stamps-note-source (hash-table-values notes)) ))
    (xref-show-xrefs locators nil)))


(defun stamps-next-in-file(&optional previous)
  (interactive)
  (when (search-forward-regexp "\\(\\[cite:@.*\\)\ \\(.*\\)\\]" nil t (if previous -1 1))
    (let* ((match1 (match-beginning 1))
	   (match2 (match-end 2))
	   (str (and match1 match2 (buffer-substring-no-properties
				    match1 match2)))
	   (file (buffer-file-name))
	   (citekey-at-point  (stamps-extract-citekey str))
	   (container  (or (stamps-get-from-table citekey-at-point 'containers)
			   (stamps-load-citekey citekey-at-point))))
      (cl-multiple-value-bind (locator timestamp precise-locator type)
	  (stamps-extract-from-summary str)
	(let ((w (or (stamps-get-pdf-window)
		     (stamps-get-other-window))))
	  (with-selected-window w
	    (stamps-goto-pdf-page citekey-at-point locator precise-locator)))))))


(defun stamps-next-in-active-container ()
  (interactive)
  (let* ((container stamps-active-container)
	 (active-note (stamps-container-active-note container))
	 (number-of-notes (stamps-container-number-of-notes container))
	 (new-index (mod (1+ active-note) number-of-notes))
	 (note (seq-elt  (stamps-container-notes container) new-index)))
    (stamps-goto-note note 'file)))

(defun stamps-next-mpv-in-file(&optional previous)
  (interactive)
  (when (search-forward-regexp stamps-timestamp-stamp-regexp nil t )
    (let* ((citekey (substring-no-properties (match-string 1)))
	   (match2 (substring-no-properties (match-string 2)))
	   (match3 (substring-no-properties (match-string 3)))
	   (resources (stamps-get-resources-from-citekey citekey))
	   (container  (or (stamps-get-from-table  citekey 'containers)
			   (stamps-load-citekey citekey)))
	   (path (mpv-get-property "path"))
	   (file (car  (-filter
			(lambda (s)
			  (string-match-p match2 s))
			resources ))))
      (when file
	(unless (equal path file)
	  (mpv-play file))
	 (mpv-seek match3)))))

(defun stamps-goto-pdf-page (citekey page coords)
  (interactive)
  ;; NOTE: Only one file is supported for pdf files.
  (when-let ((file (car (stamps-get-resources-from-citekey citekey)))
	     (page (if (stringp page) (string-to-number page) page)))
    (find-file file)
    (when (and page (equal major-mode 'pdf-view-mode))
      (pdf-view-goto-page page)
      (when coords
	(pdf-view-deactivate-region)
	(setq pdf-view-active-region
	      (list coords))
	(when (pdf-view-active-region-p)
	  (pdf-view-display-region))))))

(defun stamps-goto-next-in-pdf (&optional previous)
  (interactive)
  (when-let* ((container (stamps-get-container))
	      (active-note (stamps-container-active-note container))
	      (number-of-notes (stamps-container-number-of-notes container))
	      (new-index (min (1+ active-note) (1- number-of-notes)))
	      (note (aref (stamps-container-notes container) new-index)))
    (setf (stamps-container-active-note container) new-index)
    (setf stamps-active-container container)
    (message "%s/%s" (1+ new-index) number-of-notes)
    (when stamps-mode
      (stamps-goto-note note 'file))
    (stamps-goto-note note 'document)))

(defun stamps-goto-previous-in-pdf (&optional previous)
  (interactive)
  (when-let* ((container (stamps-get-container))
	      (active-note (stamps-container-active-note container))
	      (number-of-notes (stamps-container-number-of-notes container))
	      (new-index (max (1- active-note) 0))
	      (note (aref (stamps-container-notes container) new-index)))
    (setf (stamps-container-active-note container) new-index)
    (setf stamps-active-container container)
    (message "%s/%s" (1+ new-index) number-of-notes)
    (when stamps-mode
      (stamps-goto-note note 'file))
    (stamps-goto-note note 'document)))

(defun stamps-goto-last-in-pdf (&optional previous)
  (interactive)
  (when-let* ((container (stamps-get-container))
	      (number-of-notes (stamps-container-number-of-notes container))
	      (new-index (1- number-of-notes))
	      (note (aref (stamps-container-notes container) new-index)))
    (setf (stamps-container-active-note container) new-index)
    (setf stamps-active-container container)
    (message "%s/%s" (1+ new-index) number-of-notes)
    (when stamps-mode
      (stamps-goto-note note 'file))
    (stamps-goto-note note 'document)))

(defun stamps-goto-first-in-pdf (&optional previous)
  (interactive)
  (when-let* ((container (stamps-get-container))
	      (number-of-notes (stamps-container-number-of-notes container))
	      (new-index 0)
	      (note (aref (stamps-container-notes container) new-index)))
    (setf (stamps-container-active-note container) new-index)
    (setf stamps-active-container container)
    (message "%s/%s" (1+ new-index) number-of-notes)
    (when stamps-mode
      (stamps-goto-note note 'file))
    (stamps-goto-note note 'document)))

(defun stamps-goto-note (note where)
  (cl-case where
    (file
     (let ((source (stamps-note-source note)))
       (if (equal 'regexp (stamps-note-type note))
	   (xref--show-location (xref-item-location source) t)
	 (with-selected-window (stamps-get-note-window)
	   (xref--show-location (xref-item-location source) t)))))
    (document
     (let* ((page (stamps-note-locator note))
	    (summary (xref-item-summary (stamps-note-source note)))
	    (citekey (stamps-extract-citekey summary))
	    (coords (stamps-note-precise-locator note)))
       (with-selected-window (stamps-get-pdf-window)
	 (stamps-goto-pdf-page citekey page coords))))))

(defun stamps-goto-related-note (page &optional window)
  (interactive))

;;;; Create MPV notes
(defun stamps-annotate-mpv ()
  (interactive)
  (require 'mpv)
  (let* ((timestamp (format-time-string "%H:%M:%S"
					(seconds-to-time
					 (mpv-get-playback-position))
					t))
	 (path (mpv-get-property "path"))
	 (file-or-url  (if (and path (file-regular-p path))
			   (file-name-nondirectory path)
			 path))
	 (reference (url-hexify-string file-or-url))
	 (citekey (or (stamps-get-citekey path)
		      (and
		       (message "No citekey found, NOCITEKEY placeholder will be used.")
		       "NOCITEKEY"))) )
    (when citekey
      (when (derived-mode-p 'prog-mode)
	(insert (format "%s " comment-start)))
      (insert (format stamps-timestamp-format-string citekey reference timestamp)))))

;;;; stamps-mode
(defvar stamps-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used for `stamps-mode'.")

(define-minor-mode stamps-mode
  "stamps-mode"
  :init-value nil
  :group 'stamps
  :global t
  :keymap stamps-mode-map
  (if stamps-mode
      (progn
	(add-hook 'after-save-hook 'stamps-load-file-notes)
	(advice-add 'pdf-view-goto-page :after 'stamps-goto-related-note))
    (progn
      (remove-hook 'after-save-hook 'stamps-load-file-notes)
      (advice-remove 'pdf-view-goto-page  'stamps-goto-related-note))))
