;;; remarkable-connection.el --- reMarkable API -*- lexical-binding: t -*-

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

;; The top-level API. This takes the form of a class defining the
;; basic operations, and a set of hooks and other bits to customise
;; what happens.

;;; Code:

;; ---------- GLobal variables ----------

(defvar remarkable-tablet nil
  "Connection to the reMarkable tablet.")


(defun remarkable--connection-type-selector (s)
  "Create a connection for type S.

The supported types are a USB connection, a local wifi
connection, and the reMarkable cloud. Changing connection type
will invalidate any local caches and force a reload."
  (cond ((eq s 'usb)
	 (make-instance 'remarkable-ssh-connection :target remarkable-ssh--usb-ip-address))
	((eq s 'cloud)
	 ((error "Needs porting to connection setup")))
	((stringp s)
	 (make-instance 'remarkable-ssh-connection :target s))
	(t
	 (error "Unrecognised connection type %s" s))))


;; ---------- Top-level API ----------

(defun remarkable-mode ()
  "Initialise the mode by creating a connection to the tablet.

If there is a cache file, it is loaded to re-create the
connection last in use. Otherwise a new connection is created
with type of connection determined by the value of the
`remarkable--connection-type' customisation variable and the
`remarkable--connection-type-selector' function."
  (interactive)
  (if (remarkable-cache-exists?)
      ;; load the connection from the cache
      (remarkable-load-cache)

    ;; create a new connection
    (let ((conn (remarkable--connection-type-selector remarkable--connection-type)))
      (setq remarkable-tablet conn)
      (remarkable-init remarkable-tablet)
      (remarkable-index remarkable-tablet)))


  ;; ---------- Connection class ----------

  (defclass remarkable-connection ()
    ((cache :initform nil))
    "The class of connections to the reMarkable tablet."))


(cl-defgeneric remarkable-init (conn)
  "Initialise the connection CONN.")


(cl-defmethod remarkable-init (conn)
  "By default there is no connection CONN initialisation to do."
  (message "No initialisation to do"))


(cl-defgeneric remarkable-save (conn)
  "Save the details of the connection CONN in the current buffer.

This should include the file cache anmd any connection or authentication
details.")


(cl-defgeneric remarkable-load (conn)
  "Re-build a cached connection CONN from the current buffer.")


(cl-defgeneric remarkable-index (conn)
  "Retrieve the index for the tablet CONN.

This may call `remarkable-sync' if there is already index data
cached, which should make loading faster. Returns a hierarchical
list of entries for the collections and documents.")


(cl-defgeneric remarkable-sync (conn)
  "Synchronise the connection CONN  with the actual state of the tablet.")


(cl-defgeneric remarkable-get (conn uuid content-fn &key format)
  "Retrieve the document UUID on CONN into file CONTENT_FN.

UUID must denote a document, not a collection. FORMAT should be a
format for this document as returned by `remarkable-get-formats'.
It may be omitted if there is only a single available format.")


(cl-defgeneric remarkable-get-formats (conn uuid)
  "Retrieve a list of formats for the document UUID on CONN.")


(cl-defgeneric remarkable-put (conn fn &key title collection tags)
  "Put the file names FN to the tablet CONN.

FN must be in a format accepted by the tablet.

If TITLE Is given it is used as a visible name for the new
document; otherwise a title is syntheised from FN.

If COLLECTION is given it should be the UUID of the collection
into which to store FN; otherwise FN is put into the root
collection.

If TAGS is given it should be a list of tags to be applied to the
document.

Returns the UUID of the newly-uploaded document.")


(cl-defgeneric remarkable-make-collection (conn collection &key parent)
  "Create a new COLLECTION on the tablet.

If PARENT is given it should be the UUID of the collection to
create COLLECTION inl otherwise COLLECTION is created in the root
collection.

Returns the UUID of the newly-created collection.")


(cl-defgeneric remarkable-delete (conn uuid)
  "Delete the document or collection identified by UUID..

If UUID identifies a collection, it must be empty in order to be
deleted.")


;; ---------- Hooks ----------

;; Synchronisation hooks. Each is called with the entry of the appropriate
;; document. The return value of the hook functions are ignored, meaning that
;; (for example) there's no way to stop a document being deleted, since it's
;; already happened on the device or in the cloud.

(defvar remarkable-document-added-hook nil
  "Hook called when a new document is found during synchronisation.")

(defvar remarkable-collection-added-hook nil
  "Hook called when a new collection is found during synchronisation.")

(defvar remarkable-document-deleted-hook nil
  "Hook called when a document is found to have been deleted during synchronisation.")

(defvar remarkable-collection-deleted-hook nil
  "Hook called when a collection is found to have been deleted during synchronisation.")

(defvar remarkable-document-changed-hook nil
  "Hook called when a document is found to have changed during synchronisation.

\"Changed\" includes moving within the hierarchy, being
annotated, being tagged, or any other change.")

(defvar remarkable-before-document-upload-hook nil
  "Hook called before a document is uploaded to the tablet.")

(defvar remarkable-after-document-download-hook nil
  "Hook called after a document is downloaded from the tablet.")


;; ---------- Hierarchy synchronisation ----------

(defun remarkable-connection--sync (hier index)
  "Synchronise HIER with a newly-downloaded INDEX.

Both HIER and INDEX consist of entries -- in the case of HIER,
entries with hierarchy; in the case of INDEX, a flat list.
Synchronising the two performs the following operations:

1. Extract the entries in HIER that are not in INDEX
   (the deleted entries), and remove them from HIER
   This consists of two sub-steps:

   1a. Remove the deleted documents
   1b. Then remove the deleted collections, checking
       that the collections are empty

2. Extract the entries in INDEX that are not in HIER
   (the new entries), retrieve their metadata, and add
   them at the correct point in HIER. This consists of two
   sub-steps:

   2a. Add any new collections
   2b. Add any new documents, whose parent collection must
       exist

3. Extract the entries in HIER that have different hashes
   to the corresponding entries in INDEX, download their
   metadata, and (if their parent has changed) move them
   within HIER

Each stage runs the aporopriate hook functions, allowing clients
to respond to the changes.

Return the new hierarchy."
  (cl-flet ((remove-document (h e)
	      (if (remarkable-entry-is-document? e)
		  (progn
		    (message "Document \"%s\" deleted" (remarkable-entry-name e))
		    (run-hook-with-args 'remarkable--document-deleted-hook e)
		    (remarkable--delete-entry e h))
		h))

	    (remove-collection (h e)
	      (if (remarkable-entry-is-collection? e)
		  (if (remarkable-entry-has-contents? e)
		      (error "Trying to delete a non-empty collection %s"  (remarkable-entry-name e))
		    (progn
		      (message "Collection \"%s\" deleted" (remarkable-entry-name e))
		      (run-hook-with-args 'remarkable--collection-deleted-hook e)
		      (remarkable--delete-entry e h)))
		h))

	    (add-collection (h e)
	      (if (remarkable-entry-is-collection? e)
		  (progn
		    (message "New collection \"%s\" added" (remarkable-entry-name e))
		    (run-hook-with-args 'remarkable--collection-added-hook e)
		    (remarkable--add-entry e h))
		h))

	    (add-document (h e)
	      (if (not (remarkable-entry-parent-exists? e h))
		  (error "Trying to add document to unknown collecton")
		(progn
		  (message "New document \"%s\" added" (remarkable-entry-name e))
		  (run-hook-with-args 'remarkable--document-added-hook e)
		  (remarkable--add-entry e h)))))

    ;; 1. remove deleted entries
    (let ((des (remarkable--find-deleted-entries hier index)))
      ;; 1a. remove documents
      (let ((hier1a (cl-reduce #'remove-document des
			       :initial-value hier)))

	;; 1b remove collections (if empty)
	(let ((hier1b (cl-reduce #'remove-collection des
				 :initial-value hier1a)))

	  ;; 2. add new entries
	  (let* ((nes (remarkable--find-new-entries-with-metadata hier1b index)))
	    ;; 2a. add collections
	    (let ((hier2a (cl-reduce #'add-collection nes
				     :initial-value hier1b)))

	      ;; 2a add new documents
	      (let ((hier2b (cl-reduce #'add-document nes
				       :initial-value hier2a)))

		;; don't do 3 for now
		hier2b))))))))


(defun remarkable-connection--find-deleted-entries (hier index)
  "Return the entries in HIER that are not in INDEX."
  (cl-flet ((is-deleted (e)
	      "Return E if it is deleted."
	      (let ((uuid (remarkable-entry-uuid e)))
		(if (null (remarkable--find-entry uuid index))
		    e))))

    (remarkable--mapcan-entries #'is-deleted hier)))


(defun remarkable-connection--find-new-entries (hier index)
  "Return the entries in INDEX that are not in HIER."
  (cl-flet ((is-new (e)
	      "Return E if it is new."
	      (let ((uuid (remarkable-entry-uuid e)))
		(if (null (remarkable--find-entry uuid hier))
		    e))))

    (remarkable--mapcan-entries #'is-new index)))


(defun remarkable--find-new-entries-with-metadata (hier index)
  "Return new entries with their metadata.

This function simply applies `remarkable--add-metadata' to the
results of `remarkable--find-new-entries'."
  (remarkable--add-metadata (remarkable--find-new-entries hier index)))


(defun remarkable-connection--find-changed-entries (hier index)
  "Return the entries in INDEX that are in HIER with different hashes.

The entry returned will have the UUID of an entry in HIER, with the
hash and metadata from INDEX."
  (cl-flet ((is-changed (e)
	      "Return E if it is changed."
	      (let* ((uuid (remarkable-entry-uuid e))
		     (f (remarkable--find-entry uuid hier)))
		(if (remarkable-entry-changed? e f)
		    e))))

    (remarkable--mapcan-entries #'is-changed index)))


(defun remarkable-connection--find-changed-entries-with-metadata (hier index)
  "Return changed entries with their metadata.

This function simply applies `remarkable--add-metadata' to the
results of `remarkable--find-changed-entries'."
  (remarkable--add-metadata (remarkable--find-changed-entries hier index)))


(provide 'remarkable-connection)
;;; remarkable-connection.el ends here
