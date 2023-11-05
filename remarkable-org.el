;;; remarkable-org.el --- reMarkable/org link -*- lexical-binding: t -*-

;; Copyrighqt (c) 2023 Simon Dobson <simoninireland@gmail.com>

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Version: 0.1.1
;; Keywords: hypermedia, multimedia
;; Homepage: https://github.com/simoninireland/remarkable
;; Package-Requires: ((emacs "27.2") (org "8.0") (org-roam)

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

;; Interface between org mode and the reMarkable.

;;; Code:

(require 'f)
(require 'org)


;; ---------- Constants ----------

(defconst remarkable-org--uuid-property "REMARKABLE-UUID"
  "Property used to store the attachment name and UUID of a linked document.")


;; ---------- Public API ----------

(defun remarkable-org-attach ()
  "Attach a document from the reMarkable cloud to the current node.

This will query for a document and (if there's a choice)
themdesired content type, and then dpwnload this document and
attach it."
  (interactive)
  (cl-destructuring-bind (e type)
      (remarkable-org--choose-document-type remarkable--root-hierarchy)
    (remarkable-org--attach-document e type)
    (remarkable-org--set-uuid-property e type)
    (message "Attached %s" (remarkable-entry-name e))))


(defun remarkable-org-read ()
  "Upload an attachment from the current org node to the reMarkable cloud.

This will query for an attachment if there is more than one, and
check the format is compatible with the tablet. The document's
visible name on the tablet is set to the same as the heading to
which it is attached."
  (interactive)
  (if-let* ((fn (remarkable-org--choose-attachment))
	    (heading (org-get-heading t t t t)))
      (progn
	(set-text-properties 0 (length heading) nil heading)
	(remarkable--upload-document fn :title heading)
	(message "Uploaded \"%s\"" heading))))


;; ---------- Attaching documents ----------

(defun remarkable-org--chooser (hier)
  "Return an alist of titles in HIER to UUIDs.

Only documents are presented, not collections.

We assume for now that titles are unique: otherwise we need to
disambiguate them."
  (remarkable--mapcan-entries (lambda (e)
				(if (remarkable-entry-is-document? e)
				    (cons (remarkable-entry-name e)
					  (remarkable-entry-uuid e))))
			      hier))


(defun remarkable-org--choose-document (hier)
  "Choose a document from the nested HIER. returning its entry.

This presents a `completing-read' buffer from which to choose a
document.

Return the entry."
  (if-let* ((chooser (remarkable-org--chooser hier))
	    (choice (completing-read "Document: "
				     chooser
				     nil t))
	    (uuid (cdr (assoc choice chooser))))
    (remarkable--find-entry uuid hier)))


(defun remarkable-org--choose-document-type (hier)
  "Chppse a document from HIER, requesting a type if there is a choice.

The types are extracted from `remarkable--file-types-supported'
using `remarkable--get-content-types'. If there is only one,
that type is used; if there are several, a `completing-read' buffer
is offered.

Return the document entry and chosen type."
  (if-let* ((e (remarkable-org--choose-document hier))
	    (types (remarkable--get-content-types e))
	    (type (cond ((equal (length types) 0)
			 (message "No content for %s" (remarkable-entry-name e)))
			((equal (length types) 1)
			 (car types))
			(t
			 (completing-read "Content type to attach "
					  types
					  nil t)))))
      (list e type)))


(defun remarkable-org--choose-attachment ()
  "Choose an attachment from the current node to upload.

This presents a `completing-read' buffer if there is more than
one possible choice of attachment. The attachment chosen must
have a content type that's accepted by the tablet, as defined by
`remarkable--file-types-supported'.

Return the attachment file to upload or nil."
  (if-let* ((attach-dir (org-attach-dir))
	    (fns (org-attach-file-list attach-dir))
	    (exts (remarkable--file-types-supported))
	    (allowed-fns (-filter (lambda (fn)
				    (member (f-ext fn) exts))
				  fns)))
      (cond ((equal (length allowed-fns) 0)
	     (message "No attachment of a supported content type to upload"))
	    ((equal (length allowed-fns) 1)
	     (car allowed-fns))
	    (t
	     (completing-read "Attachment to upload: "
			      allowed-fns
			      nil t)))

    ;; no attachments
    (message "No attachments to upload")
    nil))


(defun remarkable-org--attach-document (e type)
  "Attach the subfile of type TYPE of E to the current node.

This retrieves the document from the cloud and attaches it using
`org-attach-attach' using the 'cp' method to make sure the file
is safely stashed. The attachment file name will match the sub-file,
i.e., the UUID of E with extension TYPE."
  (let* ((uuid (remarkable-entry-uuid e))
	 (tmp (remarkable--create-temporary-directory-name uuid))
	 (content-fn (f-swap-ext (f-join tmp uuid) type)))
    (unwind-protect
	(progn
	  ;; create the temporary directory
	  (f-mkdir-full-path tmp)

	  ;; download the document
	  (if (remarkable--get-content e type content-fn)
	      ;; attach to the current node
	      (org-attach-attach content-fn nil 'cp)))

      ;; clean-up the temporary storage
      (if (f-exists? tmp)
	  (f-delete tmp t)))))


(defun remarkable-org--attachment-file-name (e type)
  "Return the attachment filename to use for entry E with the given TYPE.

This simply concatenates the UUID of E with the TYPE mas extension."
  (let ((uuid (remarkable-entry-uuid e)))
    (f-swap-ext uuid type)))


(defun remarkable-org--set-uuid-property (e type)
  "Store the details of E with type TYPE as a property."
  (let* ((uuid (remarkable-entry-uuid e))
	 (attachment-fn (remarkable-org--attachment-file-name e type))
	 (prop (org--property-local-values remarkable-org--uuid-property nil))
	 (elem (format "%s/%s/%s" uuid type attachment-fn))
	 (newprop (if prop
		      (concat (car prop) ";" elem)
		    elem)))
    (org-set-property remarkable-org--uuid-property newprop)))


;; ---------- Keymap ----------

;; We add the functions to the attachment commands (C-c C-a) so
;; that they're integrated with the rest of the attachment machinery
;; We add two commands:
;;
;;    r:         download a document and attach it
;;    R or C-r:  upload an attachment

(add-to-list 'org-attach-commands
	     (list (list ?r) #'remarkable-org-attach
		   "Download an attachment from the reMarkable tablet."))


(add-to-list 'org-attach-commands
	     (list (list ?R ?\C-r) #'remarkable-org-read
		   "Upload an attachment to read on the reMarkable tablet."))

(setq org-attach-commands (cdr org-attach-commands))

(provide 'remarkable-org)
;; remarkable-org.el ends here
