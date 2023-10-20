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
(require 'cl)


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


;; Constants

(defconst remarkable-sync-version 1.5
  "Supported version of the ReMarkable synchronisation protocol.")

(defconst remarkable-index-schema-version 3
  "Supported version of the ReMarkable index schema.")


;; ---------- State variables and cache ----------

(defvar remarkable--root-index nil
  "Will be deleted after debugging...")

(defvar remarkable--root-hierarchy nil
  "The collection and document hierarchy, extracted from the root index.")

(defvar remarkable--generation nil
  "Generation of files retrieved from the root.")

(defvar remarkable--hash nil
  "Hash of files retrieved.")


;; ---------- Public API ----------

(defun remarkable--file-types-supported ()
  "Return a list 0f the file types we support.

This should be extracted from `remarkable--file-types-plist'."
  (list "pdf" "epub" "rm" "lines"))


(defun remarkable-init ()
  "Initialise the link to the ReMarkable cloud.

This authenticates against the cloud (which requires a one-time
token the first time), downloads the root index, and constructs a
collection hierarchy for all the objects."
  (interactive)
  (unless (remarkable-authenticated?)
    (call-interactively remarkable-authenticate))

  (cl-destructuring-bind (hash gen index) (remarkable--get-root-index)
    (setq remarkable--root-index index)	;; for debugging
    (setq remarkable--hash hash
	  remarkable--generation gen
	  remarkable--root-hierarchy (remarkable--make-collection-hierarchy index)))

  ;; avoid the huge return value that results by default
  t)


(defun remarkable-put (fn &optional c)
  "Store the file FN into collection C

FN should be a local file name. C should be the UUID of a
collection. If C is ommitted the document is put into the root
collection.")


(defun remarkable-delete (uuid)
  "Delete the folder of collection with the given UUID.")


(defun remarkable-make-collection (fn &optional c)
  "Create a new collection FN in collection C.

If C is omitted the collection is created in the root collection.")


;; ---------- Root index API ----------

(defun remarkable--get-root-index ()
  "Retrieve the root index.

Returns a list containing the index hash, its generation, and a
flat list of index entries with metadata.

Use `remarkable--make-collection-hierarchy' to extract the
hierarchical structure of the index entries"
  (let* ((folder (remarkable--get-blob-url))
	 (hash-gen (remarkable--get-hash-generation folder))
	 (index (remarkable--get-index (car hash-gen))))
    (remarkable--add-metadata index)
    (list (car hash-gen) (cadr hash-gen) index)))


(defun remarkable--create-root-index ()
  "Create a raw index to be uploaded.

This traverses the `remarkable--root-hierarchy' to determine
hashes and sub-files.

Return a list consisting of the index and the hash."
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

    (let* ((index (with-temp-buffer
		    (insert (format "%s\n" remarkable-index-schema-version)) ;; schema version
		    (mapc #'insert-index-line remarkable--root-hierarchy)    ;; entries
		    (buffer-string)))
	   (hs (mapcar #'remarkable--entry-hash remarkable--root-hierarchy))
	   (hash (remarkable--sha256-sum hs)))
      (list index hash))))


;; ---------- Index handling ----------

(defun remarkable--get-index (hash)
  "Return the index of the object identified by HASH.

To get the index, we first acquire the blob by calling
`remarkable--get-blob', which returns the index of the folder.
This is passed to `remarkable--parse-index' to create a folder
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
   - ':length' the length of the element

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
			     :type (nth 1 fields)
			     :uuid (nth 2 fields)
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
    (-filter (lambda (e) (> (length e) 0))
	     (mapcar #'parse-entry (cdr lines)))))


(defun remarkable--create-index (fns)
  "Create an index for the files in FNS.

Each element of FNS should be either a filename or a cons cell
consisting of a filename and the filename to be used in the
index. This allows us to avoid copying large content files.

This function is essentially the dual of `remarkable--parse-index'."
  (cl-flet ((insert-index-line (fn-or-rename)
	      "Insert an index line for file FN-OR-RENAME."
	      (let* ((realname (if (listp fn-or-rename)
				   (car fn-or-rename)
				 fn-or-rename))
		     (indexname (f-filename (if (listp fn-or-rename)
						(cdr fn-or-rename)
					      fn-or-rename)))
		     (hash (remarkable--sha256-file realname))
		     (length (f-length realname)))
		(insert (format "%s:80000000:%s:0:%s\n" hash indexname length)))))

    (with-temp-buffer
      (insert (format "%s\n" remarkable-index-schema-version))
      (mapc #'insert-index-line fns)
      (buffer-string))))


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

The URL should be a blob URL for the root collection. This is
dereferenced with a \"GET\" request and returns a hash for the
index document of the folder and a generation hash, which are
returned as a list."
  (let (hash gen)
    (request url
      :type "GET"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable-user-agent)
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
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token))))
      :sync t
      :timeout 10
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq blob data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error getting blob: %s" error-thrown))))
    blob))


(defun remarkable--get-content-hash (index ext)
  "Get the hash of the raw content in INDEX in format EXT."
  (if-let* ( (ce (-first (lambda (e)
			   (if-let ((cfn (remarkable--entry-uuid e)))
			       (equal (f-ext cfn) ext)))
			 index)))
      (remarkable--entry-hash ce)))


(defun remarkable--get-content-types (index)
  "Return a list of the content types available in INDEX.

The types are a list of extensions, a sub-set of those returned by
`remarkable--file-types-supported' for which we have handlers."
  (if-let ((exts (remarkable--file-types-supported)))
      (mapcan (lambda (ext)
		(if (-first (lambda (e)
			      (if-let ((cfn (remarkable--entry-uuid e)))
				  (equal (f-ext cfn) ext)))
			    index)
		    (list ext)))
	      exts)))


(defun remarkable--get-content (hash fn)
  "Get the content associated with the document HASH into file FN.

The extension of FN will define to the type of content wanted.

Returns nil if there is no associated content of the correct
type."
  (if-let* ((index (remarkable--get-index hash))
	    (ext (f-ext fn)))
      (if (member ext (remarkable--get-content-types index))
	  (let ((contenthash (remarkable--get-content-hash index ext)))
	    (let ((coding-system-for-write 'no-conversion))
	      (with-temp-file fn
		(insert (remarkable--get-blob contenthash)))
	      t)))))


(defun remarkable--download-document (uuid fn)
  "Download the content of the document with the given UUID into FN."
  (if-let* ((e (remarkable--find-entry uuid remarkable--root-hierarchy))
	    (hash (remarkable--entry-hash e))
	    (success (remarkable--get-content hash fn)))
      ;; file successfully downloaded
      (message "Downloaded \"%s\" into %s" (remarkable--entry-name e) fn)

    ;; failed for some reason
    (error "Failed to download %s" fn)))


;; ---------- Object-level metadata management ----------

(defun remarkable--add-metadata (ls)
  "Add document-level metadata to the folder structure LS.

Each entry in LS is queried for metadata using `remarkable--get-metadata'.
If found, the metadata is added as a plist associated with
the ':metadata' tag."
  (mapc (lambda (e)
	  (if-let* ((hash (plist-get e :hash))
		    (metadata (remarkable--get-metadata hash)))
	      (plist-put e :metadata metadata)))
	ls))


(defun remarkable--get-metadata-hash (hash)
  "Get the hash of the metadata associated with HASH.

We first acquire the index associated with HASH, which for a document
will include a \".metadata\" file whose hash we return."
  (if-let* ((index (remarkable--get-index hash))
	    (meta (-first (lambda (e)
			    (if-let ((mfn (remarkable--entry-uuid e)))
				(equal (f-ext mfn) "metadata")))
			 index)))
      (remarkable--entry-hash meta)))


(defun remarkable--get-metadata (hash)
  "Get the metadata plist associated with the document HASH.

Return nil if there is no associated metadata, which typically means that
HASH identifies a collection, not a document."
  (if-let* ((metahash (remarkable--get-metadata-hash hash))
	    (raw-metadata (remarkable--get-blob metahash))
	    (metadata (json-parse-string raw-metadata
					 :object-type 'plist)))
      ;; patch the timestamp into a standard format
      (plist-put metadata :lastModified
		 (remarkable--timestamp (plist-get metadata :lastModified)))))


(defun remarkable--make-collection-hierarchy (es)
  "Construct a entry hierarchy for the list of entries ES.

This takes a entry list constructed from a flat index of the root
collection and converts it into a nested entry structure. Each collection
and sub-collection has a \":contents\" property that contains the documents
and collections within it.

We retain /all/ entries, including deleted ones. Other functions
can use `remarkable--entry-is-deleted?' to elide deleted entries
if required/"

  ;; The problem with this operation is that the hierarchy is only encoded
  ;; into object-level metadata, and the entries in an index are not
  ;; guaranteed to come out in an order that respects it, so we don't
  ;; know whether the parent of an entry will have been processed when
  ;; we encounter the child entry.
  ;;
  ;; The solution -- not elegant! -- is to defer the processing of
  ;; objects whose parents we can't find in the already-processed list,
  ;; and re-traverse these deferred entries once we've processed the
  ;; entire index. We possibly need to do this several times in really
  ;; obstructive cases, and an error in the hierarchy construction process
  ;; would give rise to an infinite recursion. This structure is built
  ;; and maintained client-side, so we check the the number of deferred
  ;; entries goes down with every sweep through the index to detect problems.
  ;;
  ;; Its possible this approach will fail for very large document stores
  ;; because of ELisp's restrictions on the depth of recursive calls. It
  ;; ight be worth re-writing it in iterative form.

  (cl-labels ((copy-entry (e)
		"Create a copy of E."
		(append e nil))

	      (find-collection (uuid es)
		"Find the folder with the given UUID in ES."
		(remarkable--find-entry uuid es))

	      (fold-entries (notdone done deferred ndeferred)
		"Fold entries from NOTDONE and DEFERRED into DONE to create hierarchy."
		(if (null notdone)
		    (if deferred
			;; we have deferred entries, check we've reduced them
			;; from last time (if there was one)
			;;
			;; ndeferred counts the number of deferred entries that
			;; we had when we recursed /on the deferred entries/, not
			;; at every recursive call: we only change ndeferred when
			;; we start a sweep the deferred list, and check that this
			;; number goes down at each such recursion
			(if (and (> ndeferred 0)
				 (>= (length deferred) ndeferred))
			    ;; number of deferred items hasn't reduced
			    (error "Problem with the entry hierarchy")

			  ;; otherwise carry on processing, passing in the
			  ;; number of the deferred items for later checking
			  (fold-entries deferred done nil (length deferred)))

		      ;; no deferred entries, we're done
		      done)

		  ;; process the next entry
		  (let ((e (copy-entry (car notdone)))
			(rest (cdr notdone)))
		    (if (or (remarkable--entry-is-deleted? e)
			    (remarkable--entry-is-in-root-collection? e))
			;; entry is in root collection, move to done
			;; deleted entries are treated as being in root
			(progn
			  (fold-entries rest (append done (list e)) deferred ndeferred))

		      ;; entry is in some other collection, search for it
		      (let ((parent (remarkable--entry-parent e)))
			;; look for parent in done
			(if-let ((p (find-collection parent done)))
			    ;; parent is in done, move to there and continue
			    (progn
			      (remarkable--add-entry-to-contents e p)
			      (fold-entries rest done deferred ndeferred))

			  ;; parent not yet processed, defer
			  (fold-entries rest done (append deferred (list e)) ndeferred))))))))

    (fold-entries es nil nil 0)))


(defun remarkable--find-entry-by-key-value (key value es)
  "Find the entry with the given VALUE in field KEY in (possibly nested) structure ES.

This looks only in the index entries, not in the metadata."
  (cl-block FIND
    (cl-labels ((check-entry (e)
		  "Check if E has the given key value"
		  (if (equal (plist-get e key) value)
		      (cl-return-from FIND e)
		    (check-contents e)))

		(check-contents (e)
		  "Check the contents of E."
		  (mapc #'check-entry
			(remarkable--entry-contents e))))

      (mapc #'check-entry es)

      ;; if we get here, we failed to find the entry
      nil)))


(defun remarkable--find-entry (uuid es)
  "Find the entry with the given UUID in (possibly nested) structure ES."
  (remarkable--find-entry-by-key-value :uuid uuid es))


(defun remarkable--find-entry-by-hash (hash es)
   "Find the entry with the given HASH in (possibly nested) structure ES"
   (remarkable--find-entry-by-key-value :hash hash es))


(defun remarkable--add-entry-to-contents (e f)
  "Add E to the contents of F.

This modifies the contents of F in-place. A contents field is added
if one does ot already exist."
  (let ((cs (plist-get f :contents)))
    (if cs
	;; existing contents, add to the list
	(plist-put f :contents (append cs (list e)))

      ;; no current contents, create a singleton
      (plist-put f :contents (list e)))))


(defun remarkable--add-entry (e es)
  "Add an entry E to ES.

E will be added in the appropriate place in the hierarchy, either
directly to root collection or the contents of its parent collection."
  (if (remarkable--entry-is-in-root-collection? e)
      ;; add entry to es
      (append es (list e))

    ;; add entry to parent's contents
    (let ((p (remarkable--find-entry (remarkable--entry-parent e) es)))
      (remarkable--add-entry-to-contents e p)
      es)))


(defun remarkable--create-entry (fn uuid metadata extrafiles)
  "Create an entry for FN.

UUID is the UUID used for FN in the cloud. METADATA is the
metadata plist that is added directly to the entry. EXTRAFILES is
the set of additional files created alongside the metadata and
the raw content.
"
  (let ((hash (remarkable--sha256-file fn))
	(len (f-length fn)))
    (list :hash hash
	  :type "DocumentType"
	  :uuid uuid
	  :subfiles (+ 2 (length extrafiles))    ;; content + metadata + others
	  :length len
	  :metadata metadata)))


(defun remarkable--prettyprint-hierarchy (es)
  "Pretty-print the hierarchy ES."
  (cl-labels ((make-indent (n)
		"Return an N-character indent of spaces."
		(make-string n ?\s))

	      (pp (e indent)
		"Pretty-print entry E and indentation level N."
		(let ((print-e (concat (make-indent indent)
				       (remarkable--entry-name e)
				       (format " (%s)"
					       (if (remarkable--entry-is-deleted? e)
						   "deleted"
						 (remarkable--entry-uuid e)))))
		      (print-es (mapcar (lambda (e) (pp e (+ indent 3))) (remarkable--entry-contents e))))
		  (apply #'concat (format "%s\n" print-e) print-es))))

    (cl-prettyprint (mapconcat (lambda (e) (pp e 0)) es))))


;; Field access for common information from entries

(defun remarkable--entry-metadata (e)
  "Return the object-level metedata associated with E."
  (plist-get e :metadata))

(defun remarkable--entry-metadata-get (e k)
  "Return the metadata field K associated with E."
  (plist-get (remarkable--entry-metadata e) k))

(defun remarkable--entry-uuid (e)
  "Return the UUID associated with E."
  (plist-get e :uuid))

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
  "Return the UUID of the parent of E.

A parent of \"\" indicates the E is in the root collection. A
parent of \"trash\" indicates a deleted file (see
`remarkable--entry-is-deleted?'."
  (remarkable--entry-metadata-get e :parent))

(defun remarkable--entry-is-in-root-collection? (e)
  "Test whether E is in the root collection.

E may represent a document or a collection. It is in the root
collection if its parent is \"\"."
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

(defun remarkable--entry-contents (e)
  "Return the list of sub-entries of E."
  (plist-get e :contents))


;; ---------- Document archives ----------

(defun remarkable--filename-to-name (fn)
  "Convert a filename FN into something hopefully more readable.

This relies on the name being meaningful in some sense. We
strip the extension and replace underscores with spaces.
(Should probably capitalise as well.)"
  (let* ((stem (file-name-sans-extension (f-filename fn)))
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


(defun remarkable--create-pagedata (fn ext)
    "Create the page data for FN with type EXT"
    "Blank\n")


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
   (let* ((url-max (remarkable--get-blob-upload-url hash))
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
			    (error "Error in getting root index upload URL for generation %s: %s" gen error-thrown))))
    (list url maxUpload)))


(defun remarkable--put-root-index (hash gen)
  "Write a new root index for HASH and GEN, returning a new generation."
  (let* ((url-max (remarkable--get-root-index-upload-url hash gen))
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
    (member ext (remarkable-file-types-supported))))


(cl-defun remarkable--upload-document (fn &optional (parent "root"))
  "Upload document FN to given PARENT.

If PARENT is omitted the document goes to the root collection."
  (let* ((uuid (remarkable--uuid))
	 (ext (f-ext fn))
	 (dir (f-dirname fn))
	 (name (f-no-ext fn))
	 (content-fn (f-swap-ext (f-join dir uuid) ext))
	 (tmp (remarkable--create-temporary-directory-name uuid))
	 (metadata-fn (f-swap-ext (f-join tmp uuid) "metadata"))
	 (metadata (remarkable--create-metadata-plist fn parent))
	 (metacontent-fn (f-swap-ext (f-join tmp uuid) "content"))
	 (metacontent (remarkable--create-content-plist "pdf"))
	 (pagedata-fn (f-swap-ext (f-join tmp uuid) "pagedata"))
	 (pagedata (remarkable--create-pagedata fn "pdf")))
    (unwind-protect
	(progn
	  ;; create the temporary directory
	  (f-mkdir-full-path tmp)

	  ;; create the metadata files of different kinds
	  (remarkable--create-json-file metadata-fn metadata)
	  (remarkable--create-json-file metacontent-fn metacontent)
	  (with-temp-file pagedata-fn
	    (insert pagedata))

	  ;; upload the component files
	  (mapc #'remarkable--put-blob (list metadata-fn
					     metacontent-fn
					     pagedata-fn
					     fn))

	  ;; upload the index for the new document
	  (let ((hash (remarkable--sha256-files (list metadata-fn
						      metacontent-fn
						      pagedata-fn
						      fn)))
		(index (remarkable--create-index (list metadata-fn
						       metacontent-fn
						       pagedata-fn
						       (cons fn content-fn)))))
	    (remarkable--put-blob-data index hash))

	  ;; add the new document to the hierarchy
	  (let ((e (remarkable--create-entry fn
					     uuid
					     metadata
					     (list metacontent-fn pagedata-fn))))
	    (remarkable--add-entry e remarkable--root-hierarchy))

	  ;; update the root index
	  (cl-destructuring-bind (index roothash) (remarkable--create-root-index)
	    ;; upload the index
	    (remarkable--put-blob-data index roothash)

	    ;; upload the new root index hash
	    (let ((newgen (remarkable--put-root-index roothash
						      remarkable--generation)))
	      ;; update our state to reflect the new root index and generation
	      (setq remarkable--hash roothash
		    remarkable--generation newgen)))

	  ;; finish the upload
	  (remarkable--upload-complete remarkable--generation)

	  ;; return the UUID of the newly-uploaded document
	  uuid)

      ;; clean-up temporary storage
      (if (f-exists? tmp)
	  (f-delete tmp t)))))


(provide 'remarkable-cloud-sync15)
;;; remarkable-cloud-sync15.el ends here
