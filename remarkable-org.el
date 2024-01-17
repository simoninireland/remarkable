;;; remarkable-org.el --- reMarkable/org link -*- lexical-binding: t -*-

;; Copyright (c) 2023--2024 Simon Dobson <simoninireland@gmail.com>

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


;; ---------- GLobal variables ----------

(defvar remarkable-org--documents nil
  "Mapping of documents synced with the reMarkable tablet.

The mapping takes the form of an alist mapping a reMarkable-side
UUID for an org id, allowing the org-side document heading to
which the document is indirectly attached. The attached document
may have originally come from the tablet (and been downloaded),
or may have originally come from the org note (and been
uploaded). Either way changes in the reMarkable-side document
will, when detected, trigger an update on the org-side.")


;; ---------- Hooks ----------

(defvar remarkable-org-attachment-tags-hook nil
  "Hook run to determine the tags added to a document being uplaodsed.

Each hook function is called with a list of strings as tags, and
the liost it rewturns is passed to the next hook function.")


;; ---------- Public API ----------

(defun remarkable-org-attach ()
  "Attach a document from the reMarkable tablet to the current node.

This will query for a document and (if there's a choice)
the desired content type, and then download this document and
attach it."
  (interactive)
  (let ((hier (remarkable-index remarkable-tablet)))
    (cl-destructuring-bind (e type)
	(remarkable-org--choose-document-type hier)

      ;; attach the document
      (remarkable-org--attach-document e type)

      ;; record the link between the heading and the document
      (let ((uuid (remarkable-entry-uuid e))
	    (id (org-entry-get (point) "ID" t)))
	(remarkable-org--associate uuid id))

      ;; update the cache
      (remarkable-save-cache)
      (message "Attached %s" (remarkable-entry-name e)))))


(defun remarkable-org-read ()
  "Upload an attachment from the current org node to the reMarkable tablet.

This will query for an attachment if there is more than one, and
check the format is compatible with the tablet. The document's
visible name on the tablet is set to the same as the heading to
which it is attached."
  (interactive)
  ;;(remarkable-index remarkable-tablet)
  (if-let* ((afn (remarkable-org--choose-attachment))
	    (attach-dir (org-attach-dir))
	    (fn (f-join attach-dir afn))
	    (heading (org-get-heading t t t t))
	    (id (org-entry-get nil "ID")))
      (progn
	;; clean the headline
	(set-text-properties 0 (length heading) nil heading)

	;; compute the tags to add to the document
	(let ((tags (remarkable-org--run-attachment-tags-hook fn id)))
	  ;; upload the document and its sub-files
	  (let ((uuid (remarkable-put remarkable-tablet fn :title heading :tags tags)))
	    (message "Uploaded \"%s\" (%s)" heading afn)

	    ;; record the association betwen UUID and headline id and attached file
	    (remarkable-org--associate uuid id afn)

	    ;; update the cache
	    (remarkable-save-cache))))))


;; ---------- Attaching documents ----------

(defun remarkable-org--chooser (hier)
  "Return an alist of titles in HIER to UUIDs.

Only documents are presented, not collections.

We assume for now that titles are unique: otherwise we need to
disambiguate them."
  (cl-flet ((add-choice (e)
	      "Add E as a choice if it is an un-deleted document."
	      (if (and (remarkable-entry-is-document? e)
		       (not (remarkable-entry-is-deleted? e)))
		  (cons (remarkable-entry-path e hier)
			(remarkable-entry-uuid e))))

	    (sort-choices (s1 s2)
	      "Sort two choices by their path."
	      (string< (car s1) (car s2))))

    (-sort #'sort-choices (remarkable--mapcan-entries #'add-choice hier))))


(defun remarkable-org--choose-document (hier)
  "Choose a document from the nested HIER. returning its entry.

This presents a `completing-read' buffer from which to choose a
document.

Return the entry of the chosen document."
  (if-let* ((chooser (remarkable-org--chooser hier))
	    (choice (completing-read "Document: "
				     chooser
				     nil t))
	    (uuid (cdr (assoc choice chooser))))
      (remarkable--find-entry uuid hier)))


(defun remarkable-org--choose-document-type (hier)
  "Choose a document from HIER, requesting a type if there is a choice.

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

Return the attachment file to upload or nil. This is a filename
in the attachment diretory, /not/ a full path."
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
	  (if (remarkable-get remarkable-connection uuid content-fn :format type)
	      ;; attach to the current node using the 'cp' method to
	      ;; take the document out of the temp directory
	      (org-attach-attach content-fn nil 'cp)))

      ;; clean-up the temporary storage
      (if (f-exists? tmp)
	  (f-delete tmp t)))))


(defun remarkable-org--attachment-file-name (e type)
  "Return the attachment filename to use for entry E with the given TYPE.

This simply concatenates the UUID of E with the TYPE as extension."
  (let ((uuid (remarkable-entry-uuid e)))
    (f-swap-ext uuid type)))


;; ---------- Tags ----------

(defun remarkable-org--run-attachment-tags-hook (fn id)
  "Run the functions hung on `remarkable-org-attachment-tags-hook'.

Each function on the hook is passed a list of strings
representing tags to be applied to the document being uploaded,
which is identified by its absolute filename FN and heading id
ID. The list it returns is passed to the next hook function, with
the result of the final function being the list that is used to
tag the document."
  (funcall-through remarkable-org-attachment-tags-hook '() fn id))


;; ---------- Associations ----------

(defun remarkable-org--associate (uuid id fn)
  "Record the association between document UUID and attachmewnt FN in heading ID."
  (add-to-list 'remarkable-org--documents (cons uuid (cons id fn))))


(defun remarkable-org--find-id-for-uuid (uuid)
  "Return the org element ID associated with document UUID."
  (assoc uuid remarkable-org--documents))



;; ---------- Keymap ----------

;; We add the functions to the attachment commands (C-c C-a) so
;; that they're integrated with the rest of the attachment machinery
;; We add two commands:
;;
;;    r:         download a document and attach it
;;    R or C-r:  upload an attachment

;; TODO: These two commands should be added as a group later in the list,
;; as it's presented in the dispatcher in order and we don't really want
;; these commands right upfront. It does mean hacking the list a bit, though.

(add-to-list 'org-attach-commands
	     (list (list ?r) #'remarkable-org-attach
		   "Download an attachment from the reMarkable tablet."))


(add-to-list 'org-attach-commands
	     (list (list ?R ?\C-r) #'remarkable-org-read
		   "Upload an attachment to read on the reMarkable tablet."))


(provide 'remarkable-org)
;; remarkable-org.el ends here
