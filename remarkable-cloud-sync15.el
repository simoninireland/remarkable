;;; remarkable-cloud-sync15.el --- Cloud Sync 1.5 API -*- lexical-binding: t -*-

;; Copyright (c) 2023 Simon Dobson <simoninireland@gmail.com>

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

;; Version 1.5 of the synchronisation interface.
;;
;; Based heavily on the interfaces provided by:
;;
;;   - https://github.com/juruen/rmapi/

;;; Code:

(require 'request)
(require 'json)


;; ---------- Web interface ----------

;; Hosts

(defconst remarkable-sync-host "https://internal.cloud.remarkable.com"
   "Remarkable cloud synchronisation server.")

;; API endpoints

(defconst remarkable-download-url "/sync/v2/signed-urls/downloads"
  "Endpoint for retrieving ReMarkable cloud documents.")

(defconst remarkable-upload-url "/sync/v2/signed-urls/uploads"
  "Endpoint for submitting documents to the ReMarkable cloud.")

(defconst remarkable-sync-complete-url "/sync/v2/sync-complete"
  "Endpoint for terminting a sync session with the ReMarkable cloud.")

;; HTTP headers

(defconst remarkable--generation-header "x-goog-generation"
  "Generation header.")

(defconst remarkable--cotent-length-range-header "x-goog-content-length-range"
  "Content length range header.")

(defconst remarkable--generation-match-header "x-goog-if-generation-match"
  "Generation match header.")


;; Accepted file types

(defconst remarkable-file-types (list "pdf" "epub" "rm")
  "List of acceptable file type extensions.")


;; ---------- Public API ----------

(cl-defun remarkable-ls (&optional uuid)
  "Return a document and folder structure for the given folder UUID.

If UUID is omitted, return the structure for the root folder."
  (let* ((folder (remarkable--get-blob-url uuid))
	 (hash-gen (remarkable--get-hash-generation folder))
	 (index (remarkable--get-index (car hash-gen))))
    index))


(defun remarkable-get (fn ls)
  "Get the file FN from folder LS.

If FN names a folder that folder is returned.

If FN names a document, the document ")


(defun remarkable-put (doc fn ls)
  "Store the file DOC as document FN in folder LS.")


(defun remarkable-delete (fn ls)
  "Delete FN from folder LS.

FN may name a document or a folder.")


(defun remarkable--make-folder (fn ls)
  "Create a new folder FN in folder LS.

Returns the new folder structure.")


;; ---------- Helper functions ----------

(defun remarkable--timestamp (ts)
  "onvert a ReMarkable timestamp to a Lisp timestamp.

The ReMarkable timestamp is measured in mulliseconds since the
epoch. We covert this to the usual Lisp list format for timestamps."
  (let ((s (/ (string-to-number ts) 1000)))
    (decode-time s )))


;; ---------- Doanload API interactions ----------

(cl-defun remarkable--get-blob-url (&optional uuid)
  "Get the access URL to a document or folder identified by UUID.

If UUID is omitted, the URL to the root folder is retrieved.

This function sends a \"POST\" method to the download endpoint
(`remarkable-download-url' on `remarkable-sync-host') passing a
JSON payload specifying the \"GET\" an item identified by a
hash. (The hash of the root folder is \"root\".) This returns
a JSON result including a field 'url that holds the blob URL
of the item.

(Note that this is a \"POST\" request to the API that includes
a \"GET\" request in its payload.)"
  (let ((body (list (cons "http_method" "GET")
		    (cons "relative_path" (or uuid "root"))))
	url)
    (request (concat remarkable-sync-host remarkable-download-url)
      :type "POST"
      :parser #'json-read
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token)))
		     (cons "Content-Type" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq url (cdr (assoc 'url data)))))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error %s" error-thrown))))
    url))


(defun remarkable--get-hash-generation (url)
  "Retrieve the hash and generation from URL.

The URL should be a blob URL for a folder. This is derefereced with
a \"GET\" request and returns a hash for the index document of the
folder and a generation hash, which are returned as a list."
  (let (index gen)
    (request url
      :type "GET"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token))))
      :sync t
      :success (cl-function (lambda (&key response data &allow-other-keys)
			      (setq gen (request-response-header response remarkable--generation-header))
			      (setq index data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error %s" error-thrown))))
    (cons index gen)))


(defun remarkable--get-blob (hash)
  "Get the blob associated with the given HASH.

This uses `remarkable--get-blob-url' to retreieve the download
URL, and dereferences this to get the blob itself."
  (let ((url (remarkable--get-blob-url hash))
	blob)
    (request url
      :type "GET"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token))))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq blob data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error %s" error-thrown))))
    blob))


(defun remarkable--get-index (hash)
  "Return the index of the folder identified by HASH.

The hash should have been acquired by `remarkable--get-hash-generation'
called on the appropriate blob URL. To get the index, we first acquire
the blob by calling `remarkable--get-blob', which returns the index of
the folder. This is passed to `remarkable--parse-index' to create a
folder structure."
  (let ((raw-index (remarkable--get-blob hash)))
    (let ((lines (s-lines raw-index)))
      ;; check that the index has the correct schema,
      ;; which is given by the first line
      (if (not (equal (string-to-number (car lines)) 3))
	  (error "Wrong schema version: %s" (car lines)))

      ;; return the parsed index, generated from the rest
      ;; of the lines
      (remarkable--parse-index (cdr lines)))))


(defun remarkable--parse-index (index)
  "Parse the lines in INDEX as the index of a collection.

INDEX shuld be a list of strings consisting of five fields separated
by colons. The elements are split-out and sanitised slightly into a plist
with elements:

   - ':hash' the hash identifying the file
   - ':type' the (index-level) file type, /not/ the object-level
     one
   - ':filename' the filename of the element, based on a
     UUID possibly with an extension
   - ':subfiles' the number of sub-files, and
   - ':length' the length of the element

Other elements may be added by other functions that process the list."
  (cl-flet ((parse-entry (entry)
	      (let ((fields (s-split ":" entry)))
		(cond ((equal (length fields) 5)
		       (list :hash (nth 0 fields)
			     :type (nth 1 fields)
			     :filename (nth 2 fields)
			     :subfiles (string-to-number (nth 3 fields))
			     :length (string-to-number (nth 4 fields))))
		      ((equal entry "")
		       nil)
		      (t
		       (error "Wrong number of fields in index entry: %s" (length fields)))))))
    (mapcar #'parse-entry index)))


;; ---------- Object-level metadata management ----------

(defun remarkable--add-metadata (ls)
  "Add document-level metadata to the folder structure LS.

Each entry in LS is queried for metadata using `remarkable--get-metadata'.
If found, the metadata is added as a plist associated with
the ':metadata' tag."
  (cl-mapc (lambda (e)
	     (if-let* ((hash (plist-get e :hash))
		       (metadata (remarkable--get-metadata hash)))
		 (plist-put e :metadata metadata)))
	   ls))


(defun remarkable--get-metadata (hash)
  "Get the metadata associated with the document HASH.

We first acquire the index associated with HASH, which for a document
will include a \".metadata\" file. Downloading this file retrieves
the metadata for the blob."
  (cl-flet ((metadata-hash (index)
	      "Return the hash of the metadata blob from INDEX, or nil"
	      (let ((meta (-filter (lambda (e)
				     (let ((fn (plist-get e :filename)))
				       (and (not (null fn))
					    (equal (f-ext fn) "metadata"))))
				   index)))
		(if (not (null meta))
		    (plist-get (car meta) :hash)))))
    (if-let* ((index (remarkable--get-index hash))
	      (metahash (metadata-hash index))
	      (raw-metadata (remarkable--get-blob metahash))
	      (metadata (json-parse-string raw-metadata
					   :object-type 'plist)))
	;; patch the timestamp into a standard format
	(plist-put metadata :lastModified
		   (remarkable--timestamp (plist-get metadata :lastModified))))))


(defun remarkable--make-folder-hierarchy (es)
  "Construct a entry hierarchy for the list of entries ES.

This takes a entry list constructed from a flat index of the root
collection and converts it into a nested entry structure. Each collection
and sub-collection has a \":contents\" property that contains the documents
and collections within it."

  ;; The problem with this operation is that the hierarchy is only encoded
  ;; into object-level metadata, and the entries in an index are not
  ;; guaranteed to come out in an order that respects it, so we don't
  ;; know whether the parent of an entry will have been processed when
  ;; we encounter the child entry.
  ;;
  ;; The solution -- not elegant! -- is to defer the processing of
  ;; objects whose parent we can't find in the already-processed list,
  ;; and re-traverse these deferred entries once we've processed the
  ;; entire index. We possibly need to do this several times in really
  ;; obstructive cases, and an error in the hierarchy construction process
  ;; at the server-side would give rise to an infinite recursion
  ;; (which we should probably guard against, although it doesn't seem
  ;; to be an issue in practice so far).

  (cl-labels ((copy-entry (e)
		"Create a copy of E."
		(append e nil))

	      (find-collection (uuid es)
		"Find the folder with the given UUID in ES."
		(remarkable--find-entry uuid es))

	      (add-entry-to-contents (e f)
		"Add E to the contents of F, creating the entry if necessary."
		(let ((cs (plist-get f :contents)))
		  (if cs
		      ;; existing contents, add to the list
		      (plist-put f :contents (append cs (list e)))

		    ;; no current contents, create a singleton
		    (plist-put f :contents (list e)))))

	      (fold-entries (notdone done deferred)
		"Fold entries from NOTDONE and DEFERRED into DONE to create hierarchy."
		(if (null notdone)
		    (if deferred
			;; we have deferred entries
			(fold-entries deferred done nil)
		      ;; no deferred entries, complete
		      done)

		  (let ((e (copy-entry (car notdone)))
			(rest (cdr notdone)))
		    (if (remarkable--is-deleted? e)
			;; deleted entry, elide
			(fold-entries rest done deferred)
		      (if (remarkable--object-is-in-root-collection? e)
			  ;; entry is in root collection, move to done
			  (progn
			    (princ "root\n")
			    (fold-entries rest (append done (list e)) deferred))

			(let ((parent (remarkable--object-parent e)))
			  ;; look for parent in done
			  (if-let ((p (find-collection parent done)))
			      ;; parent is in done, move to there and continue
			      (progn
				(add-entry-to-contents e p)
				(fold-entries rest done deferred))

			    ;; parent not yet processed, defer
			    (fold-entries rest done (append deferred (list e)))))))))))

    (fold-entries es nil nil)))


(defun remarkable--find-entry (uuid es)
  "Find the entry with the given UUID in (possibly nested) structure ES."
  (cl-block FIND
    (cl-labels ((check-entry (e)
		  "Check if E has the given UUID."
		  (if (equal (remarkable--entry-uuid e) uuid)
		      (cl-return-from FIND e)
		    (check-contents e)))
		(check-contents (e)
		  "Check the contents of E."
		  (mapc #'check-entry
			(remarkable--entry-contents e))))
      (mapc #'check-entry es)

      ;; if we get here, we failed to find the entry
      nil)))


(defun remarkable--prettyprint-hierarchy (es)
  "Pretty-print the hierarchy ES."
  (cl-labels ((make-indent (n)
		(make-string n ?\s))
	      (pp (e indent)
		(let ((print-e (concat (make-indent indent) (remarkable--entry-name e)))
		      (print-es (mapcar (lambda (e) (pp e (+ indent 3))) (remarkable--entry-contents e))))
		  (apply #'concat (format "%s\n" print-e) print-es))))
    (cl-prettyprint (mapconcat (lambda (e) (pp e 0)) es))))


;; Field access for common information

(defun remarkable--entry-metadata (e)
  "Return the object-level metedata associated with E."
  (plist-get e :metadata))

(defun remarkable--entry-metadata-get (e k)
  "Return the metadata field K associated with E."
  (plist-get (remarkable--entry-metadata) k))

(defun remarkable--entry-uuid (e)
  "Return the UUID associated with E."
  (plist-get e :filename))

(defun remarkable--entry-hash (e)
  "Return the hash associated with E."
  (plist-get e :hash))

(defun remarkable--entry-changed? (e1 e2)
  "Check whether E1 and E2 have the same hash."
  (equal (remarkable--entry-hash e1)
	 (remarkable--entry-hash e2)))

(defun remarkable--entry-parent (e)
  "Return the hash of the parent of E.

A parent of \"\" indicates the E is in the root folder. A parent
of \"trash\" indicates a deleted file (see `remarkable--is-deleted?'."
  (remarkable--entry-metadata-get e :parent))

(defun remarkable--entry-is-in-root-collection? (e)
  "Test whether E is in the root collection.

E may represent a document or a collection. It is in the root collection
if its parent is \"\"."
  (let ((p (remarkable--entry-parent e)))
    (or (null p)
	(equal p ""))))

(defun remarkable--entry-name (e)
  "Return the visible name associated with E."
  (remarkable--entry-metadata-get e :visibleName))

(defun remarkable--entry-is-collection? (e)
  "Check whether E is a collection (folder)."
   (equal (remarkable--entry-metadata-get e :type) "CollectionType"))

(defun remarkable--entry-is-document? (e)
  "Check whether E is a document."
    (equal (remarkable--entry-metadata-get e :type) "DocumentType"))

(defun remarkable--entry-is-deleted? (e)
  "Check whether E has been deleted."
  (equal (remarkable--entry-metadata-get e :parent) "trash"))

(defun remarkable--entry-is-not-deleted? (e)
  "Check whether E has not been deleted.

This is simply the negation of `remarkable--is-deleted?'."
  (not (remarkable--entry-is-deleted? e)))

(defun remarkable--entry-contents (e)
  "Return the list of sub-entries of E."
  (plist-get e :contents))


;; ---------- Document archives ----------

(defun remarkable--temporary-directory-name ()
  "Create a name for a temporary directory..

Returns the full path to the directory."
  (let ((dir (concat "doc" (number-to-string (random 10000)))))
    (f-join (temporary-file-directory) dir)))


(defun remarkable--create-temporary-directory ()
  "Create a temporary diretory into which to build a document archive.

Returns the full path to the directory."
  (let ((dir (remoarkable--temporary-directory-name)))
    (f-mkdir-full-path dir)
    dir))


;; ---------- Upload API interactions ----------

(defun remarkable--file-type-supported? (fn)
  "Check that the file type of FN is supported.

Supported file type extension are given in `remarkable-file-types'."
  (let ((ext (f-ext fn)))
    (member ext remarkable-file-types)))


(defun remarkable--upload-document (fn uuid)
  "Upload document FN with given UUID."
  (let ((uuid (remarkable--uuid)))



    )
  )



(provide 'remarkable-cloud-sync15)
;; remarkable-cloud-sync15.el ends here
