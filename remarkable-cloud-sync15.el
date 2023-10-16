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

(defconst remarkable--content-length-range-header "x-goog-content-length-range"
  "Content length range header.")

(defconst remarkable--generation-match-header "x-goog-if-generation-match"
  "Generation match header.")

;; Accepted file types

(defconst remarkable-file-types (list "pdf" "epub" "rm")
  "List of acceptable file type extensions.")


;; ---------- State variables and cache ----------

(defvar remarkable--root-index nil
  "The index of the root.")

(defvar remarkable--root-hierarchy nil
  "The collection and document hierarchy, extracted from the root index.")

(defvar remarkable--generation nil
  "Generation of files retrieved from the root.")

(defvar remarkable--hash nil
  "Hash of files retrieved.")


;; ---------- Public API ----------

(cl-defun remarkable-ls (&optional uuid)
  "Return a document and folder structure for the given folder UUID.

If UUID is omitted, return the structure for the root folder."
  (remarkable--get-root-index))


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


;; ---------- Root index API ----------

(defun remarkable--get-root-index ()
  "Retrieve and install the root index."
  (let* ((folder (remarkable--get-blob-url))
	 (hash-gen (remarkable--get-hash-generation folder))
	 (index (remarkable--get-index (car hash-gen))))
    (setq remarkable--hash (car hash-gen))
    (setq remarkable--generation (cdr hash-gen))
    (remarkable--add-metadata index)
    (setq remarkable--root-index index)
    (setq remarkable--root-hierarchy (remarkable--make-collection-hierarchy index))))


(defun remarkable--hash-root-index ()
  "Return the hash of the root index.

This is simply the sum of the individual objects' hashes."
  (cl-flet ((entry-hash (e)
	      "Return the hash of the entry E."
	      (plist-get e :hash)))
    (let ((hs (mapcar #'entry-hash remarkable--root-index)))
      (remarkable--sha256-sum hs))))


(defun remarkable--create-root-index ()
  "Create a raw index to be uploaded.

This traverses the `remarkable--root-hierarchy' to determine
hashes and sub-files."
  (cl-labels ((insert-index-line (e)
	      "Insert an index line for entry E."
	      (when e
		(let ((hash (remarkable--entry-hash e))
		      (fn (remarkable--entry-uuid e))
		      (length (remarkable--entry-length e))
		      (subfiles (remarkable--entry-subfiles e))
		      (contents (remarkable--entry-contents e)))
		  (insert (format "%s:80000000:%s:%s:%s\n" hash fn subfiles length))
		  (if contents
		      (mapc #'insert-index-line contents))))))

    (with-temp-buffer
      (insert (format "%s\n" remarkable-index-schema-version))
      (mapc #'insert-index-line remarkable--root-hierarchy)
      (buffer-string))))


;; ---------- Index handling ----------

(defun remarkable--get-index (hash)
  "Return the index of the folder identified by HASH.

The hash should have been acquired by `remarkable--get-hash-generation'
called on the appropriate blob URL. To get the index, we first acquire
the blob by calling `remarkable--get-blob', which returns the index of
the folder. This is passed to `remarkable--parse-index' to create a
folder structure."
  (let* ((raw-index (remarkable--get-blob hash))
	 (lines (s-lines raw-index)))
    (remarkable--parse-index lines)))


(defun remarkable--parse-index (lines)
  "Parse the LINES as the index of a collection.

LINES should have the scheme version as the first line, followed by
a list of strings consisting of five fields separated by colons.
The elements are split-out and sanitised slightly into a plist
with elements:

   - ':hash' the hash identifying the file
   - ':type' the (index-level) file type, /not/ the object-level
     one
   - ':filename' the filename of the element, based on a
     UUID possibly with an extension
   - ':subfiles' the number of sub-files, and
   - ':length' the length of the element

Other elements may be added tho entries by other functions.
In particular, `remarkable--get-metada' adds object-level metadata
to the entry, and `remarkable--make-collection-hierarchy' forms
the collection-document hierarchy.

This function is the dual of `remarkable--create-index'."
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
    ;; check that the index has the correct schema,
    ;; which is given by the first line
    (if (not (equal (string-to-number (car lines)) remarkable-index-schema-version))
	(error "Wrong schema version: %s" (car lines)))

    ;; generate entries for the rest of the lines
    (mapcar #'parse-entry (cdr lines))))


(defun remarkable--create-index (fns)
  "Create an index for the files in FNS.

This the dual of `remarkable--parse-index'."
  (cl-flet ((insert-index-line (fn)
	      "Insert an index line for file FN."
	      (let ((hash (remarkable--sha256-file fn))
		    (length (f-length fn)))
		(insert (format "%s:80000000:%s:0:%s\n" hash fn length)))))

    (with-temp-buffer
      (insert (format "%s\n" remarkable-index-schema-version))
      (mapc #'insert-index-line fns)
      (buffer-string))))


;; ---------- Download API interactions ----------

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
			    (error "Error getting blob URL: %s" error-thrown))))
    url))


(defun remarkable--get-hash-generation (url)
  "Retrieve the hash and generation from URL.

The URL should be a blob URL for a folder. This is dereferenced with
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
			    (error "Error getting document hash and generation: %s" error-thrown))))
    (list index gen)))


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
      :timeout 10
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq blob data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error getting blob: %s" error-thrown))))
    blob))


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


(defun remarkable--make-collection-hierarchy (es)
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
  ;; would give rise to an infinite recursion (which we should probably
  ;; guard against, since the whole thing is done client-side).

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

	      (fold-entries (notdone done deferred ndeferred)
		"Fold entries from NOTDONE and DEFERRED into DONE to create hierarchy."
		(if (null notdone)
		    (if deferred
			;; we have deferred entries, check we've reduced them
			(if (equal (length deferred) ndeferred)
			    ;; the number of deferred entries is the same as we started with
			    (error "Problem with the hierarchy")

			  ;; otherwise carry on processing
			  (fold-entries deferred done nil (length deferred)))

		      ;; no deferred entries, complete
		      done)

		  (let ((e (copy-entry (car notdone)))
			(rest (cdr notdone)))
		    (if (remarkable--is-deleted? e)
			;; deleted entry, elide
			(fold-entries rest done deferred)
		      (if (remarkable--object-is-in-root-collection? e)
			  ;; entry is in root collection, move to done
			  (fold-entries rest (append done (list e)) deferred ndeferred)

			(let ((parent (remarkable--object-parent e)))
			  ;; look for parent in done
			  (if-let ((p (find-collection parent done)))
			      ;; parent is in done, move to there and continue
			      (progn
				(add-entry-to-contents e p)
				(fold-entries rest done deferred) ndeferred)

			    ;; parent not yet processed, defer
			    (fold-entries rest done (append deferred (list e)) ndeferred)))))))))

    (fold-entries es nil nil 0)))


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
		(let ((print-e (concat (make-indent indent)
				       (remarkable--entry-name e)
				       (format " (%s)" (remarkable--entry-uuid e))))
		      (print-es (mapcar (lambda (e) (pp e (+ indent 3))) (remarkable--entry-contents e))))
		  (apply #'concat (format "%s\n" print-e) print-es))))
    (cl-prettyprint (mapconcat (lambda (e) (pp e 0)) es))))


;; Field access for common information from entries

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

(defun remarkable--entry-length (e)
  "Return the length of E."
  (plist-get e :length))

(defun remarkable--entry-subfiles (e)
  "Return the number of sub-files associated with E."
  (plist-get e :subfiles))

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

(defun remarkable--filename-to-name (fn)
  "Convert a filename FN into something hopefully more readable.

This relies on the name being meaningful in some sense. We
strip the extension and replace underscores with spaces.
(Should probably capitalise as well.)"
  (let* ((stem (file-name-sans-extension fn))
	 (spaced (string-replace "_" " " stem)))
    spaced))


(defun remarkable--create-metadata-plist (fn parent)
  "Create a metadata plist for FN going into PARENT."
  (list :visibleName (remarkable--filename-to-name fn)
	:type "DocumentType"
	:parent parent
	:synced t
	:lastModified (remarkable--lisp-timestamp (current-time))))


(defun remarkable--create-content-plist (ext)
  "Create the content description for a document of type EXT."
  (list :dummyDocument: :false
	:fileType: ext
	:pageCount: 0
	:lastOpenedPage: 0
	:lineHeight: -1
	:margins: 180
	:orientation "portrait"
	:textScale: 1
	:transform (list :m11 1
			 :m12 0
			 :m13 0
			 :m21 0
			 :m22 1
			 :m23 0
			 :m31 0
			 :m32 0
			 :m33 1)
	:pages nil
	:pageTage: nil
	:extraMetatata: (list :lastPen: "Finelinerv2"
			      :lastTool: "Finelinerv2"
			      :lastFinelinerv2Size: 1)))


(defun remarkable--create-json-file (json-fn plist)
  "Write PLIST value JSON to JSON-FN."
  (let ((json (json-encode plist)))
    (with-temp-file json-fn
      (insert json)
      (json-pretty-print (point-min) (point-max)))))


;; ---------- Upload API interactions ----------

(defun remarkable--get-blob-upload-url (hash)
  "Get the upload URL and maximum upload size for a document with the given HASH.

This function sends a \"POST\" method to the download endpoint
(`remarkable-upload-url' on `remarkable-sync-host') passing a
JSON payload specifying the \"PUT\" an item identified by a
hash.

(Note that this is a \"POST\" request to the API that includes
a \"PUT\" request in its payload.)"
  (let ((body (list (cons "http_method" "PUT")
		    (cons "relative_path" hash)))
	url maxUpload)
    (request (concat remarkable-sync-host remarkable-upload-url)
      :type "POST"
      :parser #'json-read
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token)))
		     (cons "Content-Type" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq url (cdr (assoc 'url data)))
			      (setq maxUpload (cdr (assoc 'maxuploadsize_bytes data)))))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error in getting blob upload URL: %s" error-thrown))))
    (list url maxUpload)))


(defun remarkable--put-blob-data (data &optional hash)
   "Upload a blob of DATA, using HASH if provided.

This works by hashing DATA (if HASH is omitted) and acquiring
an upload URL for that hash using `remarkable--get-blob-upload-url',
and then making a \"PUT\" request against this URL to upload DATA."
   (unless hash
     (setq hash (remarkable--sha256-data data)))
   (let* ( (url-max (remarkable--get-blob-upload-url hash))
	  (url (car url-max))
	  (maxUpload (cadr url-max)))
     (request url
       :type "PUT"
       ;; :files (list (cons "upload" fn))
       :data data
       :parser #'buffer-string
       :headers (list (cons "User-Agent" remarkable-user-agent)
		      (cons remarkable--content-length-range-header (format "0,%s" maxUpload))
		      (cons "Authorization" (concat "Bearer " (remarkable-token))))
       :sync t
       :success (cl-function (lambda (&key response &allow-other-keys)
			       (message "Uploaded data (%s bytes)" (length data))))
       :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			     (error "Error in uploading data: %s" error-thrown))))
     t))


(defun remarkable--put-blob (fn &optional hash)
  "Upload the file FN, using HASH if provided.

This uses `remarkable--put-blob-data' to send the file."
  (let ((data (with-temp-buffer
		(let ((coding-system-for-write 'no-conversion))
		  (insert-file-contents fn))
		(buffer-string))))
    (remarkable--put-blob-data data hash)
    (message "Uploaded %s" fn)))


(defun remarkable--get-root-index-upload-url (hash gen)
  "Get the upload URL and maximum upload size for the root index.

HASH is the hash of the new index for generation GEN.

This function sends a \"POST\" method to the download endpoint
(`remarkable-upload-url' on `remarkable-sync-host') passing a
JSON payload specifying the \"PUT\" an item identified by a
hash.

(Note that this is a \"POST\" request to the API that includes
a \"PUT\" request in its payload.)"
  (let ((body (list (cons "http_method" "PUT")
		    (cons "relative_path" "root")
		    (cons "root_schema" root)
		    (cons "generation" gen)))
	url maxUpload)
    (request (concat remarkable-sync-host remarkable-upload-url)
      :type "POST"
      :parser #'json-read
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token)))
		     (cons "Content-Type" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq url (cdr (assoc 'url data)))
			      (setq maxUpload (cdr (assoc 'maxuploadsize_bytes data)))))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error in getting root index upload URL: %s" error-thrown))))
    (list url maxUpload)))


(defun remarkable--put-root-index (hash gen)
  "Write a new root index for HASH and GEN, returning a new generation."
  (let* ( (url-max (remarkable--get-root-index-upload-url hash gan))
	  (url (car url-max))
	  (maxUpload (cadr url-max))
	  newgen)
     (request url
       :type "PUT"
       :data hash
       :parser #'buffer-string
       :headers (list (cons "User-Agent" remarkable-user-agent)
		      (cons remarkable--generation-match-header gen)
		      (cons remarkable--content-length-range-header (format "0,%s" maxUpload))
		      (cons "Authorization" (concat "Bearer " (remarkable-token))))
       :sync t
       :success (cl-function (lambda (&key response &allow-other-keys)
			       (setq newgen (request-response-header response remarkable--generation-header))
			       (message "Uploaded root index (new generation %s)" newgen)))
       :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			     (error "Error in uploading root index: %s" error-thrown))))
     newgen))


(defun remarkable--upload-complete (gen)
  "Indicate to the ReMarkable cloud that the upload of GEN is finished.

This performs a \"POST\" request against the completion endpoint
(`remarkable-sync-complete-url' on `remarkable-sync-host')."
  (let ((body (list (cons "generation" gen))))
    (request (concat remarkable-sync-host remarkable-sync-complete-url)
      :type "POST"
      :parser #'buffer-string
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token)))
		     (cons "Content-Type" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      t))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error finishing upload: %s" error-thrown))))))


(defun remarkable--file-type-supported? (fn)
  "Check that the file type of FN is supported.

Supported file type extension are given in `remarkable-file-types'."
  ;; Change to use custom options
  (let ((ext (f-ext fn)))
    (member ext remarkable-file-types)))


(cl-defun remarkable--upload-document (fn &optional (parent "root"))
  "Upload document FN to given PARENT.

If PARENT is omitted the document goes to the root collection."
  (let* ((uuid (remarkable--uuid))
	 (ext (f-ext fn))
	 (name (f-no-ext fn))
	 (tmp (remarkable--create-temporary-directory-name uuid))
	 (content-fn (f-swap-ext (f-join tmp uuid) "pdf"))
	 (metadata-fn (f-swap-ext (f-join tmp uuid) "metadata"))
	 (metadata (remarkable--create-metadata-plist fn parent))
	 (metacontent-fn (f-swap-ext (f-join tmp uuid) "content"))
	 (metacontent (remarkable--create-content-plist "pdf")))
    (unwind-protect
	(progn
	  ;; create the temporary directory
	  (f-mkdir-full-path tmp)

	  ;; copy-in the content
	  (f-copy fn content-fn)

	  ;; create the metadata files of different kinds
	  (remarkable--create-json-file metadata-fn metadata)
	  (remarkable--create-json-file metacontent-fn metacontent)

	  ;; upload the component files
	  (mapc #'remarkable--put-blob (list metadata-fn
					     metacontent-fn
					     content-fn))

	  ;; upload the index
	  (let ((hash (remarkable--sha256-files (list metadata-fn
						      metacontent-fn
						      content-fn)))
		(index (remarkable--create-index (list metadata-fn
						       metacontent-fn
						       content-fn))))
	    (remarkable--put-blob-data index hash))

	  ;; add the new document to the root index
	  (remarkable--add-document-to-root-index uuid hash metadata)

	  ;; update the root index
	  (let* ((roothash (remarkable--hash-root-index))
		 (newgen (remarkable--put-root-index hash remarkable--generation)))
	    (setq remarkable--hash roothash)
	    (setq remarkable--generation newgen))

	  ;; finish the upload
	  (remarkable--upload-complete remarkable--generation))

      ;; clean-up temporary storage
      (if (f-exists? tmp)
	  (f-delete tmp t)))))


(provide 'remarkable-cloud-sync15)
;;; remarkable-cloud-sync15.el ends here
