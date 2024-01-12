;;; remarkable-cloud-sync15.el --- Cloud Sync 1.5 API -*- lexical-binding: t -*-

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

;; Version 1.5 of the synchronisation interface.
;;
;; Based heavily on the interfaces provided by:
;;
;;   - https://github.com/juruen/rmapi/

;;; Code:

(require 'dash)
(require 'request)
(require 'json)
(require 'cl-lib)


;; ---------- Web interface ----------

;; Hosts

(defconst remarkable--sync-host "https://internal.cloud.remarkable.com"
  "Remarkable cloud synchronisation server.")

;; API endpoints

(defconst remarkable--download-url "/sync/v2/signed-urls/downloads"
  "Endpoint for retrieving ReMarkable cloud documents.")

(defconst remarkable--upload-url "/sync/v2/signed-urls/uploads"
  "Endpoint for submitting documents to the ReMarkable cloud.")

(defconst remarkable--sync-complete-url "/sync/v2/sync-complete"
  "Endpoint for terminting a sync session with the ReMarkable cloud.")

;; HTTP headers

(defconst remarkable--generation-header "x-goog-generation"
  "Generation header.")

(defconst remarkable--content-length-range-header "x-goog-content-length-range"
  "Content length range header.")

(defconst remarkable--generation-match-header "x-goog-if-generation-match"
  "Generation match header.")


;; Constants

(defconst remarkable--sync-version 1.5
  "Supported version of the ReMarkable synchronisation protocol.")

(defconst remarkable--index-schema-version 3
  "Supported version of the ReMarkable index schema.")

(defconst remarkable--index-file-type 80000000
  "Type used in indices to mark \"real\" files")

(defconst remarkable--index-subfile-type 0
  "Type used in indices to mark sub-files")


;; ---------- State variables and cache ----------

;; These values are all cached: see `remarkable-save-cache' and `remarkable-load-cache'.

(defvar remarkable--root-hierarchy nil
  "The collection and document hierarchy, extracted from the root index.")

(defvar remarkable--generation nil
  "Generation of the root index.")

(defvar remarkable--hash nil
  "Hash of the root index.")


;; ---------- Public API ----------

(defun remarkable-init ()
  "Initialise the link to the reMarkable cloud.

This authenticates against the cloud (which requires a one-time
token the first time), downloads the root index, and constructs a
collection hierarchy for all the objects."
  (interactive)
  (unless (remarkable-authenticated?)
    (call-interactively #'remarkable-authenticate))
  (remarkable-sync))


(defun remarkable-sync ()
  "Load the root index from the reMarkable cloud.

The root index is used to build the local view of the collection and
document hierarchy. It is cached to speed up processing."
  (interactive)
  (cl-destructuring-bind (hash gen hier)
      (remarkable--get-root-index)

    ;; update the local state to reflect the new index
    (setq remarkable--hash hash
	  remarkable--generation gen
	  remarkable--root-hierarchy hier)

    ;; save the cache
    (remarkable-save-cache)))


(defun remarkable-get (uuid type fn)
  "Download content of type TYPE for document UUID into FN."
  (let ((e (remarkable--find-entry uuid remarkable--root-hierarchy)))
    (if  (remarkable--get-content e type fn)
	;; file successfully downloaded
	(message "Downloaded \"%s\" into %s" (remarkable-entry-name e) fn)

      ;; failed for some reason
      (error "Failed to download %s" fn))))


(cl-defun remarkable-put (fn &optional c)
  "Store the file FN into collection C

FN should be a local file name. C should be the UUID of a
collection. If C is ommitted the document is put into the root
collection.")


(defun remarkable-delete (uuid)
  "Delete the folder of collection with the given UUID.")


(cl-defun remarkable-make-collection (fn &optional c)
  "Create a new collection FN in collection C.

If C is omitted the collection is created in the root collection.")


;; ---------- Root index API ----------

(defun remarkable--get-root-index ()
  "Retrieve the root index.

Returns a list containing the index hash, its generation, and a
flat list of index entries with metadata.

If we don't have a local index, download it and its metadata. If
the index has changed since we last downloaded it, synchronise
our local copy."
  (cl-flet ((full-build (index)
	      "Do a full build of INDEX."
	      (let ((withmeta (remarkable--add-metadata index)))
		(remarkable--make-collection-hierarchy withmeta)))

	    (sync-build (index)
	      "Synchronise the local state with INDEX."
	      (remarkable--sync remarkable--root-hierarchy index)))

    (cl-destructuring-bind (hash gen index)
	(remarkable--get-bare-root-index)
      (let ((hier (cond ((null remarkable--root-hierarchy)
			 (message "Rebuilding local documents")
			 (full-build index))
			((remarkable--root-index-has-changed? hash gen)
			 (message "Synchronising local documents")
			 (sync-build index))
			(t
			 (message "Local documents in sync with cloud")
			 remarkable--root-hierarchy))))

	(list hash gen hier)))))


(defun remarkable--root-index-has-changed? (hash gen)
  "Test whether the root index is the same as the local version.

HASH and GEN are values returned by`remarkable--get-bare-root-index'.
The root index changes when its hash and generation change."
  (not (and (equal hash remarkable--hash)
	    (equal gen remarkable--generation))))


(defun remarkable--get-bare-root-index ()
  "Retrieve the root index \"bare\", without metadata."
  (let ((root (remarkable--get-blob-url)))
    (cl-destructuring-bind (hash gen) (remarkable--get-hash-generation root)
      (let ((index (remarkable--get-index hash)))
	(list hash gen index)))))


(defun remarkable--create-root-index (hier)
  "Create a raw index from HIER, suitable to be uploaded.

The index entries are all to real files (not sub-files), and
all therefore have length 0 (since lengths are only maintained
for sub-files).

Return a list consisting of the index and the hash."
  (cl-labels ((insert-index-line (e)
		"Insert an index line for entry E."
		(when e
		  (let ((hash (remarkable-entry-hash e))
			(fn (remarkable-entry-uuid e))
			(subfiles (remarkable-entry-subfiles e))
			(contents (remarkable-entry-contents e)))
		    (insert (format "%s:%s:%s:%s:0\n"
				    hash remarkable--index-file-type fn subfiles))
		    (if contents
			(mapc #'insert-index-line contents))))))

    (let* ((index (with-temp-buffer
		    (insert (format "%s\n" remarkable--index-schema-version))
		    (mapc #'insert-index-line hier)
		    (buffer-string)))
	   (hs (remarkable--entry-hashes hier))
	   (hash (remarkable--sha256-sum hs)))
      (list index hash))))


;; ---------- Index handling ----------

(defun remarkable--get-index (hash)
  "Return the index of the object identified by HASH.

To get the index, we first acquire the blob by calling
`remarkable--get-blob', which returns the index of the folder.
This is passed to `remarkable--parse-index' to create an entry
structure."
  (let* ((raw-index (remarkable--get-blob hash))
	 (lines (s-lines raw-index)))
    (remarkable--parse-index lines)))


(defun remarkable--parse-index (lines)
  "Parse the LINES as the index of a collection.

LINES should have the schema version as the first line, followed
by a list of strings consisting of five fields separated by
colons. The elements are split-out and sanitised slightly into a
plist with elements:

   - ':hash' the hash identifying the file
   - ':type' the (index-level) file type, /not/ the object-level
     one
   - ':uuid' the filename of the element, based on a
     UUID, possibly with an extension for a sub-file
   - ':subfiles' the number of sub-files, and
   - ':length' the length of the element, which is always 0
     for a file, but a real length for a sub-file

Other elements may be added to entries by other functions. In
particular, `remarkable--get-metada' adds object-level metadata
to the entry, and `remarkable--make-collection-hierarchy' forms
the collection-document hierarchy.

This function is the dual of `remarkable--create-index'."
  (cl-flet ((parse-entry (entry)
	      "Split-up and clean-up ENTRY."
	      (let ((fields (s-split ":" entry)))
		(cond ((equal (length fields) 5)
		       (list :hash (nth 0 fields)
			     :type (string-to-number (nth 1 fields))
			     :uuid (nth 2 fields)
			     :subfiles (string-to-number (nth 3 fields))
			     :length (string-to-number (nth 4 fields))))
		      ((equal entry "")
		       nil)
		      (t
		       (error "Wrong number of fields in index entry: %s" (length fields)))))))

    ;; check that the index has the correct schema,
    ;; which is given by the first line
    (if (not (equal (string-to-number (car lines)) remarkable--index-schema-version))
	(error "Wrong schema version: %s" (car lines)))

    ;; generate entries for the rest of the lines
    (-filter (lambda (e) (> (length e) 0))
	     (mapcar #'parse-entry (cdr lines)))))


(defun remarkable--create-index (fns)
  "Create an index for the files in FNS.

This function is intended for constructing the index for uploaded
documents, which consist of the raw content plus supporting
metadata. We refer to these files collectively as \"sub-files\".

This function is essentially the dual of `remarkable--parse-index'
for sub-files."
  (cl-flet ((insert-index-line (fn)
	      "Insert an index line for file FN."
	      (let ((hash (remarkable--sha256-file fn))
		    (basename (f-filename fn))
		    (length (f-length fn)))

		(insert (format "%s:%s:%s:0:%s\n"
				hash remarkable--index-subfile-type basename length)))))

    (with-temp-buffer
      (insert (format "%s\n" remarkable--index-schema-version))
      (mapc #'insert-index-line fns)
      (buffer-string))))


;; ---------- Hierarchy synchronisation ----------

(defun remarkable--sync (hier index)
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


(defun remarkable--find-deleted-entries (hier index)
  "Return the entries in HIER that are not in INDEX."
  (cl-flet ((is-deleted (e)
	      "Return E if it is deleted."
	      (let ((uuid (remarkable-entry-uuid e)))
		(if (null (remarkable--find-entry uuid index))
		    e))))

    (remarkable--mapcan-entries #'is-deleted hier)))


(defun remarkable--find-new-entries (hier index)
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


(defun remarkable--find-changed-entries (hier index)
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


(defun remarkable--find-changed-entries-with-metadata (hier index)
  "Return changed entries with their metadata.

This function simply applies `remarkable--add-metadata' to the
results of `remarkable--find-changed-entries'."
  (remarkable--add-metadata (remarkable--find-changed-entries hier index)))


;; ---------- Download API interactions ----------

(cl-defun remarkable--get-blob-url (&optional hash)
  "Get the access URL to a document or folder identified by HASH.

If HASH is omitted, the URL to the root folder is retrieved.

This function sends a \"POST\" method to the download endpoint
(`remarkable-download-url' on `remarkable-sync-host') passing a
JSON payload specifying the \"GET\" an item identified by a
hash. (The hash of the root folder is \"root\".) This returns a
JSON result including a field 'url' that holds the download URL
for the blob. A \"GET\" request against this URL will retrieve
its contents

(Note that this is a \"POST\" request to the API that includes
a \"GET\" request in its payload.)"
  (let ((body (list (cons "http_method" "GET")
		    (cons "relative_path" (or hash "root"))))
	url)
    (request (concat remarkable--sync-host remarkable--download-url)
      :type "POST"
      :parser #'json-read
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable--user-agent)
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

The URL should be a blob URL for the root collection. This is
dereferenced with a \"GET\" request and returns a hash for the
index document of the folder and a generation hash, which are
returned as a list."
  (let (hash gen)
    (request url
      :type "GET"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable--user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token))))
      :sync t
      :success (cl-function (lambda (&key response data &allow-other-keys)
			      (setq gen (request-response-header response remarkable--generation-header))
			      (setq hash data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error getting document hash and generation: %s" error-thrown))))
    (list hash gen)))


(defun remarkable--get-blob (hash)
  "Get the blob associated with the given HASH.

This uses `remarkable--get-blob-url' to retreieve the download
URL, and dereferences this to get the blob itself."
  (let ((url (remarkable--get-blob-url hash))
	blob)
    (request url
      :type "GET"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable--user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token))))
      :sync t
      :timeout 10
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq blob data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error getting blob: %s" error-thrown))))
    blob))


(defun remarkable--get-content-types (e)
  "Return a list of the content types available for entry E.

The types are a list of extensions, a sub-set of those returned by
`remarkable--file-types-supported' for which we have handlers."
  (let* ((hash (remarkable-entry-hash e))
	 (index (remarkable--get-index hash))
	 (exts (remarkable--file-types-supported)))
    (mapcan (lambda (sf)
	      (let* ((cfn (remarkable-entry-uuid sf))
		     (ext (f-ext cfn)))
		(if (member ext exts)
		    (list ext))))
	    index)))


(defun remarkable--get-content-type (e type)
  "Return the entry of the subfile for content type TYPE on entry E."
  (let* ((hash (remarkable-entry-hash e))
	 (index (remarkable--get-index hash)))
      (-first (lambda (sf)
		(let* ((cfn (remarkable-entry-uuid sf))
		       (ext (f-ext cfn)))
		  (equal ext type)))
	      index)))


(defun remarkable--get-content (e type fn)
  "Get the content of type TYPE for entry E into file FN.

Returns nil if there is no associated content of the correct
type."
  (if-let* ((sf (remarkable--get-content-type e type))
	    (hash (remarkable-entry-hash sf)))
      (let ((coding-system-for-write 'no-conversion))
	(with-temp-file fn
	  (insert (remarkable--get-blob hash)))
	t)))


;; ---------- Object-level metadata management ----------

(defun remarkable--add-metadata (es)
  "Add document-level metadata to the entries ES.

Each entry in ES is queried for metadata using `remarkable--get-metadata'.
If found, the metadata is added as a plist associated with
the ':metadata' tag."
  (mapc (lambda (e)
	  (princ (format "%s\n" e))
	  (condition-case nil
	      (if-let* ((hash (remarkable-entry-hash e))
			(metadata (remarkable--get-metadata hash)))
		  (plist-put e :metadata metadata))
	    (error nil)))
	es))


(defun remarkable--get-metadata-hash (index)
  "Get the hash of the metadata associated with INDEX."
  (if-let* ((meta (-first (lambda (e)
			    (if-let ((mfn (remarkable-entry-uuid e)))
				(equal (f-ext mfn) "metadata")))
			 index)))
      (remarkable-entry-hash meta)))


(defun remarkable--get-metadata (hash)
  "Get the metadata plist associated with the document HASH.

Return nil if there is no associated metadata, which shouldn't
happen."
  (if-let* ((index (remarkable--get-index hash))
	    (metahash (remarkable--get-metadata-hash index))
	    (raw-metadata (remarkable--get-blob metahash)))
      (json-parse-string raw-metadata
			 :object-type 'plist)))



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
    (request (concat remarkable--sync-host remarkable--upload-url)
      :type "POST"
      :parser #'json-read
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable--user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token)))
		     (cons "Content-Type" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq url (cdr (assoc 'url data)))
			      (setq maxUpload (cdr (assoc 'maxuploadsize_bytes data)))))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error in getting blob upload URL: %s" error-thrown))))
    (list url maxUpload)))


(cl-defun remarkable--put-blob-data (data &optional hash)
   "Upload a blob of DATA, using HASH if provided.

This works by hashing DATA (if HASH is omitted) and acquiring
an upload URL for that hash using `remarkable--get-blob-upload-url',
and then making a \"PUT\" request against this URL to upload DATA."
   (cl-destructuring-bind (url maxUpload)
       (remarkable--get-blob-upload-url (if hash
					    hash
					  (remarkable--sha256-data data)))
     (princ (format "putting data to %s" (if hash
					    hash
					  (remarkable--sha256-data data))))
     (request url
       :type "PUT"
       ;; :files (list (cons "upload" fn))
       :data data
       :parser #'buffer-string
       :headers (list (cons "User-Agent" remarkable--user-agent)
		      (cons remarkable--content-length-range-header (format "0,%s" maxUpload))
		      (cons "Authorization" (concat "Bearer " (remarkable-token))))
       :sync t
       :success (cl-function (lambda (&key response &allow-other-keys)
			       (message "Uploaded data (%s bytes)" (length data))))
       :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			     (error "Error in uploading data: %s" error-thrown))))
     t))


(cl-defun remarkable--put-blob (fn &optional hash)
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

HASH is the hash of the new index for current generation GEN.

This function sends a \"POST\" method to the download endpoint
(`remarkable-upload-url' on `remarkable-sync-host') passing a
JSON payload specifying the \"PUT\" on the root with the new
index hash and current generation.

(Note that this is a \"POST\" request to the API that includes
a \"PUT\" request in its payload.)"
  (let ((body (list (cons "http_method" "PUT")
		    (cons "relative_path" "root")
		    (cons "root_schema" hash)
		    (cons "generation" (string-to-number gen))))
	url maxUpload)
    (request (concat remarkable--sync-host remarkable--upload-url)
      :type "POST"
      :parser #'json-read
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable--user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token)))
		     (cons "Content-Type" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq url (cdr (assoc 'url data)))
			      (setq maxUpload (cdr (assoc 'maxuploadsize_bytes data)))))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error in getting root index upload URL for generation %s: %s" gen error-thrown))))
    (list url maxUpload)))


(defun remarkable--put-root-index (hash gen)
  "Write a new root index for HASH and GEN, returning a new generation."
  (cl-destructuring-bind (url maxUpload) (remarkable--get-root-index-upload-url hash gen)
    (let (newgen)
      (request url
	:type "PUT"
	:data hash
	:parser #'buffer-string
	:headers (list (cons "User-Agent" remarkable--user-agent)
		       (cons remarkable--generation-match-header (string-to-number gen))
		       (cons remarkable--content-length-range-header (format "0,%s" maxUpload))
		       (cons "Authorization" (concat "Bearer " (remarkable-token))))
	:sync t
	:success (cl-function (lambda (&key response &allow-other-keys)
				(setq newgen (request-response-header response remarkable--generation-header))
				(message "Uploaded root index (new generation %s)" newgen)))
	:error (cl-function (lambda (&key error-thrown &allow-other-keys)
			      (error "Error in uploading root index: %s" error-thrown))))
      newgen)))


(defun remarkable--upload-complete (gen)
  "Indicate to the ReMarkable cloud that the upload of GEN is finished.

This performs a \"POST\" request against the completion endpoint
(`remarkable-sync-complete-url' on `remarkable-sync-host')."
  (let ((body (list (cons "generation" (string-to-number gen)))))
    (request (concat remarkable--sync-host remarkable--sync-complete-url)
      :type "POST"
      :parser #'buffer-string
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable--user-agent)
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
  (let ((ext (f-ext fn)))
    (member ext (remarkable--file-types-supported))))


(defun remarkable--upload-document-subfiles (fs)
  "Upload the subfiles FS and create a document file index.

Return the hash of the uploaded document."
  (let* ((sorted-files (sort fs #'string<))
	 (hash (remarkable--sha256-files sorted-files))
	 (index (remarkable--create-index fs)))

    ;; upload the subfiles
    (mapc #'remarkable--put-blob sorted-files)

    ;; upload the index for the new document
    (remarkable--put-blob-data index hash)

    ;; return the hash
    hash))


(defun remarkable--upload-root-index (hier)
  "Upload a new root index to the cloud and update the local state."

  (cl-destructuring-bind (rootindex roothash)
      (remarkable--create-root-index hier)

    ;; upload the root index to its new hash
    (remarkable--put-blob-data rootindex roothash)

    ;; update the root index
    (let ((newgen (remarkable--put-root-index roothash
					      remarkable--generation)))
      ;; finish the upload
      (remarkable--upload-complete newgen)

      ;; update the local state to reflect the new root
      ;; index and generation
      (setq remarkable--root-hierarchy hier
	    remarkable--hash roothash
	    remarkable--generation newgen))))


(cl-defun remarkable--upload-document (fn &key parent title)
  "Upload document FN to given PARENT.

If PARENT is omitted the document goes to the root collection.
If TITLE is supplied it is used as the visible name for the
document."
  (let* ((uuid (remarkable--uuid))
	 (tmp (remarkable--create-temporary-directory-name uuid)))
    (unwind-protect
	(progn
	  ;; create temporary directory
	  (f-mkdir-full-path tmp)

	  ;; create the metadata and sub-files
	  (cl-destructuring-bind (metadata fns)
	      (remarkable--create-subfiles fn uuid tmp
					   :parent parent
					   :title title)

	    ;; upload the document and its sub-files
	    (let* ((content-fn (car fns))  ; first sub-file
		   (other-fns (cddr fns))  ; other sub-files excluding metadata
		   (hash (remarkable--upload-document-subfiles fns))

		   ;; create entry and new hierarchy
		   (e (remarkable--create-entry content-fn
						uuid
						hash
						metadata
						other-fns))
		   (hier (remarkable--add-entry e remarkable--root-hierarchy)))

	      ;; update the root index
	      (remarkable--upload-root-index hier)

	      ;; return the UUID of the newly-created document
	      uuid)))

      ;; clean-up temporary storage
      (if (f-exists? tmp)
	  (f-delete tmp t)))))


;; ---------- Delete API interactions ----------

(defun remarkable--delete-blob (uuid)
  "Delete the document or collection UUID.

This is the basic operation that simply removes a blob. It
presents as an API call, although it actually isn't: there's no
delete operation in the API. Instead we simply re-build the root
index minus the index line for the selected blob."
  (let ((e (remarkable--find-entry uuid remarkable--root-hierarchy)))
    ;; delete the document from the hierarchy
    (let ((hier (remarkable--delete-entry e remarkable--root-hierarchy)))
      (cl-destructuring-bind (rootindex roothash) (remarkable--create-root-index hier)
	(princ (format "old %s\nnew %s\n" remarkable--hash roothash))
	;; upload the index to its new hash
	(remarkable--put-blob-data rootindex roothash)
	(princ "done\n")

	;; upload the new root index hash
	(let ((newgen (remarkable--put-root-index roothash
						  remarkable--generation)))
	  (princ (format "oldgen %s\nnewgen %s\n" remarkable--generation newgen))
	  ;; finish the upload
	  (remarkable--upload-complete newgen)

	  ;; update our state to reflect the new root index and generation
	  (setq remarkable--root-hierarchy hier
		remarkable--hash roothash
		remarkable--generation newgen))))))


(defun remarkable--delete-document (uuid)
  "Delete the document identified by UUID.

This will delete the document and all its components."
  (let ((e (remarkable--find-entry uuid remarkable--root-hierarchy)))
    (when (remrkable-entry-is-collection? e)
      (error "UUID %s is a collection" uuid))

    ;; delete the contents
    (let ((es (remarkable-entry-contents e)))
      ))


  )

(provide 'remarkable-cloud-sync15)
;;; remarkable-cloud-sync15.el ends here
