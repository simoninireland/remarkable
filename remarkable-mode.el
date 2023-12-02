;;; remarkable-mode.el --- Tablet buffer -*- lexical-binding: t -*-

;; Copyright (c) 2023 Simon Dobson <simoninireland@gmail.com>

;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A buffer for interfacing with tablet documents.

;;; Code:

;; ---------- Configuration ----------

(defconst remarkable-mode--buffer-name "*remarkable*"
  "Name for file interaction buffer.")


(defconst remarkable-mode--metadata-text-property 'metadata
  "Property added to metadata text.")


;; ---------- User-level interaction ----------



;; ---------- Display functions ----------

(defun remarkable-mode--show-buffer (es)
  "Show a file buffer for the (possibly hierarchical) entries in ES."
  (interactive)
  (let ((buf (get-buffer-create remarkable-mode--buffer-name)))
    (set-buffer buf)

    (let ((inhibit-read-only t)) ;; in case we're re-using the buffer
      ;; delete any contents
      (erase-buffer)

      ;; insert files
      (remarkable-mode--create-file-list es)
      (goto-char (point-min)))

    ;; make buffer read-only and mark as unmodified
    (read-only-mode t)
    (not-modified)

    ;; place buffer into remarkable-mode
    (remarkable-mode)

    ;; display the buffer
    (display-buffer buf)))


(defun remarkable-mode--make-indent (n)
  "Return an N-character indent of spaces."
  (make-string n ?\s))


(cl-defun remarkable-mode--make-entry-icon (e &optional blank)
  "Create the icon for entry E.

If blank is non-nil, the icon is inserted as blanks to
preserve indentation."
  (cond (blank
	 "    ")

	;; collection
	((remarkable-entry-is-collection? e)
	 "[col] ")

	;; document
	(t
	 "[doc] ")))


(defun remarkable-mode--create-file-list (es)
  "Create an interactive file list for hierarchical entries ES in the current buffer."
  (cl-labels ((pp (e indent)
		 "Pretty-print entry E and indentation level N."
		 (unless (remarkable-entry-is-deleted? e)
		   ;; main title line
		   (insert (remarkable-mode--make-indent indent)
			   (remarkable-mode--make-entry-icon e)
			   (let ((n (remarkable-entry-name e)))
			     (if n
				 n
			       "???")))
		   (if (remarkable-entry-is-collection? e)
		       (insert "/"))
		   (insert "\n")

		   ;; contents
		   (let ((print-es (mapcan (lambda (e)
					     (pp e (+ indent 3)))
					   (remarkable-entry-contents e))))
		     (if print-es
			 (insert (funcall #'concat print-es)))))))

    (mapc (lambda (e)
	    (pp e 0))
	  es)))


(defun remarkable-mode--entry-at-point ()
  "Return the entry at point.

This assumes that every entry occupies one line. If we change
this, for example to expand metadata, this will need to be
changed."
  (interactive)
  (let ((line (line-number-at-pos)))
    (remarkable--find-entry-n line remarkable--root-hierarchy)))


;; ---------- Keymap ----------

(defvar remarkable-mode-map (define-keymap
			      "n" #'forward-line
			      "p" #'backward-line
			      "q" #'quit-window)
  "Keymap for interacting with ReMarkable entries.")


;; ---------- Mode hooks ----------

(defvar remarkable-mode-hook nil
  "Hook run when a reMarkable mode dired-style buffer is opened.")


;; ---------- Mode ----------

(define-derived-mode remarkable-mode fundamental-mode "reMarkable"
  "Dired-like interface to the reMarkable tablet.

The following commands are available:

\\{remarkable-mode-map}")


(provide 'remarkable-mode)
;;; remarkable-mode.el ends here
