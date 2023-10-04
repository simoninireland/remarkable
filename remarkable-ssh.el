;;; remarkable-ssh.el --- ReMarkable by wifi -*- lexical-binding: t -*-

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

;; An API based on having ssh access to the ReMarkable tablet.
;; This works both over wifi and over a local USB network.

;;; Code:

(require 'dash)
(require 'org)
(require 'json)


;; ---------- Parameters ----------

(defconst remarkable-doc-store-path "~/.local/share/remarkable/xochitl"
  "Path from the home directory to the document store.")

(defconst remarkable-user "root"
  "Tablet username.")

(defvar remarkable-host "remarkable"
  "The hostname or IP address for the tablet.")


;; ---------- Helper functions ----------

(cl-defun remarkable--tramp-doc-store-path (&optional (p ""))
  "Return the Tramp path to a fle P in the document store.

The path is to the root of the document store if P is nil."
  (format "/sshx:%s@%s:%s%s" remarkable-user remarkable-host remarkable-doc-store-path p))


(defun remarkable--tramp-metadata-path (uuid)
  "Return the Tramp path to the metadata file for document UUID."
  (remarkable--tramp-doc-store-path (concat "/" uuid ".metadata")))


(defun remarkable--tramp-pdf-path (uuid)
  "Return the Tramp path to the PDF file (if it exists) for document UUID."
  (remarkable--tramp-doc-store-path (concat "/" uuid ".pdf")))


(defun remarkable--tramp-epub-path (uuid)
  "Return the Tramp path to the EPUB file (if it exists) for document UUID."
  (remarkable--tramp-doc-store-path (concat "/" uuid ".epub")))


;; ---------- Metadata cache construction ----------

(defun remarkable--get-documents ()
  "Retrieve a list of documents from the tablet.

This looks for metadata files and then massages the filenames
to construct a list of UUIDs."
  (let* ((path (remarkable--tramp-doc-store-path))
	 (files (f-files path))
	 (metadata-files (-filter (lambda (fn)
				    (equal (f-ext fn) "metadata"))
				  files)))
    (mapcar (lambda (f) (f-no-ext (f-relative f path)))
	    metadata-files)))


(defun remarkable--get-document-metadata-alist (uuid)
  "Retrieve the metadata for the document UUID."
  (with-temp-buffer
    (insert-file-contents (remarkable--tramp-metadata-path uuid))
    (goto-char 0)
    (json-read)))


(defun remarkable--get-document-format (uuid)
  "Infer the document format for UUID.

This may be PDF or EPUB. The format is inferred by the existence
of files with the appropriate extensions."
  (cond ((f-exists? (remarkable--tramp-epub-path uuid))
	 "EPUB")
	((f-exists? (remarkable--tramp-pdf-path uuid))
	 "PDF")
	(t "UNKNOWN")))


(defun remarkable--extend-document-metadata (uuid m)
  "Extend the metadata M from the tablet for UUID.

We add a new field:

   - 'format: the format of the document"
  (append m (list (cons'format (remarkable--get-document-format uuid)))))


(defun remarkable--get-metadata-alist ()
  "Retrieve the metadata for all the documents, returning an alist.

The alist maps document UUIDs to that document's metadata alist. This
is essentially a full metadata description of the tablet's document store.
It includes any deleted items."
  (let ((uuids (remarkable--get-documents)))
    (mapcar (lambda (uuid)
	      (let* ((m (remarkable--get-document-metadata-alist uuid))
		     (mx (remarkable--extend-document-metadata uuid m)))
		(cons uuid mx)))
	    uuids)))


(defun remarkable--all-uuids (meta)
  "Return all the UUIDs in META.

This is a helper function for `remarkable--uuids', which allows filtering."
  (mapcar #'car meta))


(cl-defun remarkable--uuids (meta &key (documents t) (folders t) (deleted nil))
  "Return a list of all the UUID in META.

By default this returns all non-deleted documents and folders. Setting
DOCUMENTS to 'nil' will ignore documents; setting FOLDERS to
'nil' will ignore folders; and setting DELETED to 't' will include
deleted items"
  (cl-flet ((include-uuid (uuid)
	      "True if we should include UUID in the list"
	      (or (and documents (remarkable--is-document? uuid meta))
		  (and folders (remarkable--is-folder? uuid meta)))))
    (let ((candidates (if deleted
			  (remarkable--all-uuids meta)
			(-filter (lambda (uuid)
				   (not (remarkable--is-deleted? uuid meta)))
				 (remarkable--all-uuids meta)))))
      (-filter #'include-uuid candidates))))


;; ---------- Metadata access ----------

(defun remarkable--get-document-metadata (uuid k meta)
  "Return the value of field K for document UUID in META."
  (cdr (assoc k (cdr (assoc uuid meta)))))


(defun remarkable--document-name (uuid meta)
  "Return the name of the folder or document UUID in META."
  (remarkable--get-document-metadata uuid 'visibleName meta-prefix-charq))


(defun remarkable--document-format (uuid meta)
  "Return the format of the document UUID in META."
  (remarkable--get-document-metadata uuid 'format meta))


(defun remarkable--document-parent (uuid meta)
  "Return the name of the parent folder of the document or folder UUID in META.

This is 'nil' if the parent is top-level, and \"trash\" if it
has been deleted."
  (remarkable--get-document-metadata uuid 'parent meta))


(defun remarkable--is-document? (uuid meta)
  "True if document UUID in META is a document."
  (equal (remarkable--get-document-metadata uuid 'type meta) "DocumentType"))


(defun remarkable--is-folder? (uuid meta)
  "True if document UUID in META is a folder."
  (equal (remarkable--get-document-metadata uuid 'type meta) "CollectionType"))


(defun remarkable--is-deleted? (uuid meta)
  "True if document UUID in META has been deleted."
  (equal (remarkable--document-parent uuid meta) "trash"))



(provide 'remarkable-ssh)
;; remarkable-ssh.el ends here
