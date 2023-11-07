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


;; ---------- Hooks ----------

;; Synchronisation hooks. Each is called with the entry of the appropriate
;; document. The return value of the hook functions is ignored, meaning that
;; (for example) there's no way to stop a document being deleted, since it's
;; already happened in the cloud.

(defvar remarkable--document-added-hook nil
  "Hook called when a new document is found during synchronisation.")

(defvar remarkable--collection-added-hook nil
  "Hook called when a new collection is found during synchronisation.")

(defvar remarkable--document-deleted-hook nil
  "Hook called when a document is found to have been deleted during synchronisation.")

(defvar remarkable--collection-deleted-hook nil
  "Hook called when a collection is found to have been deleted during synchronisation.")

(defvar remarkable--document-changed-hook nil
  "Hook called when a document is found to have changed during synchronisation.

\"Changed\" includes moving within the hierarchy, being
annotated, being tagged, or any other change.")


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
  "Load the index from the reMarkable cloud.

The index is used to build the local view of the collection and
document hierarchy."
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


;; ---------- File type handling ----------

(defun remarkable--file-types-supported ()
  "Return a list of the file types we support.

This should be extracted from `remarkable--file-types-plist'."
  (list "pdf" "epub" "rm" "lines"))


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
  "Get the content of type TYE for entry E into file FN.

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


(defun remarkable--make-collection-hierarchy (es)
  "Construct a entry hierarchy for the list of entries ES.

This takes a entry list constructed from a flat index of the root
collection and converts it into a nested entry structure. Each collection
and sub-collection has a \":contents\" property that contains the documents
and collections within it.

We retain /all/ entries, including deleted ones. Other functions
can use `remarkable-entry-is-deleted?' to elide deleted entries
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
  ;; might be worth re-writing it in iterative form.

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
		    (if (or (remarkable-entry-has-no-metadata? e)
			    (remarkable-entry-is-deleted? e)
			    (remarkable-entry-is-in-root-collection? e))
			;; entry is in root collection, move to done
			;; deleted entries and those without metadata
			;; are treated as being in root
			(progn
			  (fold-entries rest (append done (list e)) deferred ndeferred))

		      ;; entry is in some other collection, search for it
		      (let ((parent (remarkable-entry-parent e)))
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
			(remarkable-entry-contents e))))

      (mapc #'check-entry es)

      ;; if we get here, we failed to find the entry
      nil)))


(defun remarkable--find-entry (uuid es)
  "Find the entry with the given UUID in (possibly nested) structure ES."
  (remarkable--find-entry-by-key-value :uuid uuid es))


(defun remarkable--find-entry-by-hash (hash es)
   "Find the entry with the given HASH in (possibly nested) structure ES"
   (remarkable--find-entry-by-key-value :hash hash es))


(defun remarkable--mapcan-entries (f es)
  "Map F across the (possibly nested) entries in ES.

F should be a function taking an entry as argument.

Return a flat list of results, so that the hierarchy isn't
exposed. This makes it easier to perform global operations
on the cache."
  (cl-labels ((mapf (e)
		"Return a list of values for E, including those of its contents."
		(let ((h (funcall f e))
		      (chs (mapcan #'mapf (remarkable-entry-contents e))))
		  (if h
		      (cons h chs)
		    chs))))

    (mapcan #'mapf es)))


(defun remarkable--entry-hashes (es)
  "Extract all the hashes from the entries in ES."
  (remarkable--mapcan-entries #'remarkable-entry-hash es))


(defun remarkable--entry-uuids (es)
  "Extract all the UUIDs from the  entries in ES."
  (remarkable--mapcan-entries #'remarkable-entry-uuid es))


(defun remarkable--find-entry-n (n es)
  "Return entry E from the (possibly nested) entries in ES.

This performs a traversal of the entry hierarchy, visiting an
entry and then all its contents recursively. N starts from 1.
Deleted entries are skipped."
  (cl-block FOUND
    (cl-labels ((find-entry (m hier)
		  "Return entry N from HIER."
		  (if (null hier)
		      (1- m)
		    (let ((e (car hier)))
		      (cond ((remarkable-entry-is-deleted? e)
			     (find-entry m (cdr hier)))
			    ((equal m n)
			     (cl-return-from FOUND e))
			    (t
			     (let ((contents (remarkable-entry-contents e)))
			       (if contents
				   (setq m (find-entry (1+ m) contents))))
			     (find-entry (1+ m) (cdr hier))))))))

      (find-entry 1 es)

      ;; if we get here, we didn't find the entry
      (error "No entry %s" n))))


(defun remarkable--add-entry-to-contents (e f)
  "Add E to the contents of F.

This modifies the contents of F in-place. A :contents field is added
if one does ot already exist."
  (let ((cs (plist-get f :contents)))
    (if cs
	;; existing contents, add to the list
	(plist-put f :contents (append cs (list e)))

      ;; no current contents, create a singleton
      (plist-put f :contents (list e)))))


(defun remarkable--delete-entry-from-contents (e f)
  "Remove entry E from the contents of entry F."
  (let ((cs (plist-get f :contents)))
    (if cs
	;; remove from the list
	(plist-put f :contents (-remove-item e cs))

      ;; can't remove from an entry without contents
      (error "Entry %s has no contents to remove from" (remarkable-entry-uuid f)))))


(defun remarkable--add-entry (e es)
  "Add an entry E to ES.

E will be added in the appropriate place in the hierarchy, either
directly to the root collection or the contents of its parent
collection."

  ;; don't add duplicate entries
  (let ((uuid(remarkable-entry-uuid e) ))
    (if (remarkable--find-entry uuid es)
	(error "Entry already exists for UUID %s" uuid)))

  (if (remarkable-entry-is-in-root-collection? e)
      ;; add entry to es
      (append es (list e))

    ;; add entry to parent's contents
    (let ((p (remarkable--find-entry (remarkable-entry-parent e) es)))
      (remarkable--add-entry-to-contents e p)
      es)))


(defun remarkable--delete-entry (e es)
  "Remove the entry E from ES, retruning the new hierarchy."
  (if (remarkable-entry-is-in-root-collection? e)
      ;; remove directly
      (-remove-item e es)

    ;; remove from parent :contents
    (let ((p (remarkable--find-entry (remarkable-entry-parent e) es)))
      (remarkable--delete-entry-from-contents e p)
      es)))


(defun remarkable--create-entry (fn uuid hash metadata extrafiles)
  "Create an entry forFN with UUID with hash HASH.

METADATA is the metadata plist that is added directly to the
entry. EXTRAFILES is the set of additional files created
alongside the metadata and the raw content."
  (let ((len (f-length fn)))
    (list :hash hash
	  :type remarkable--index-file-type
	  :uuid uuid
	  :subfiles (+ 2 (length extrafiles)) ;; content + metadata + others
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
				       (remarkable-entry-name e)
				       (format " (%s)"
					       (cond ((remarkable-entry-has-no-metadata? e)
						      (format  "no metdata: %s %s"
							       (remarkable-entry-uuid e)
							       (remarkable-entry-subfiles e)))
						     ((remarkable-entry-is-deleted? e)
						      "deleted")
						     (t
						      (remarkable-entry-uuid e))))))
		      (print-es (mapcar (lambda (e) (pp e (+ indent 3)))
					(remarkable-entry-contents e))))
		  (apply #'concat (format "%s\n" print-e) print-es))))

    (cl-prettyprint (mapconcat (lambda (e)
				 (pp e 0)) es))))


;; Field access for common information from entries

(defun remarkable-entry-metadata (e)
  "Return the object-level metedata associated with E."
  (plist-get e :metadata))


(defun remarkable-entry-metadata-get (e k)
  "Return the metadata field K associated with E."
  (plist-get (remarkable-entry-metadata e) k))


(defun remarkable-entry-uuid (e)
  "Return the UUID associated with E.

This is actually a misnomer. The UUID field is indeed a UUID for
\"real\" files (documents and collections); but for sub-files it
is a filename with the UUID as a stem, with an extension and
possibly other distinguishing information. So calling this function
on entries taken from the cache or main index will indeed return
a UUID, but calling it on sib-file entries (such as those obtained
from `remarkable-entry-subfiles') will return a filename."
  (plist-get e :uuid))


(defun remarkable-entry-hash (e)
  "Return the hash associated with E."
  (plist-get e :hash))


(defun remarkable-entry-length (e)
  "Return the length of E.

This is only really meaningful when applied to a sub-file,
since the lengths og \"real\" files are always 0."
  (plist-get e :length))


(defun remarkable-entry-is-file? (e)
  "Test whether E is a real file.

\"Real\" files are those representing documents or collections."
  (equal (plist-get e :type) remarkable--index-file-type))


(defun remarkable-entry-is-subfile? (e)
  "Test whether E is a sub-file.

Sub-files are those representing raw content, metadata,
annotations, and other components of documents (\"real\" files)."
  (equal (plist-get e :type) remarkable--index-subfile-type))


(defun remarkable-entry-subfiles (e)
  "Return the number of sub-files associated with E."
  (plist-get e :subfiles))


(defun remarkable-entry-changed? (e1 e2)
  "Check whether E1 and E2 have the same hash."
  (equal (remarkable-entry-hash e1)
	 (remarkable-entry-hash e2)))

(defun remarkable-entry-parent (e)
  "Return the UUID of the parent of E.

A parent of \"\" indicates the E is in the root collection. A
parent of \"trash\" indicates a deleted file (see
`remarkable-entry-is-deleted?'."
  (remarkable-entry-metadata-get e :parent))


(defun remarkable-entry-is-in-root-collection? (e)
  "Test whether E is in the root collection.

E may represent a document or a collection. It is in the root
collection if its parent is \"\"."
  (let ((p (remarkable-entry-parent e)))
    (or (null p)
	(equal p ""))))


(defun remarkable-entry-parent-exists? (e es)
  "Test whether the parent of E exists in ES.

The root collection always exists."
  (or (remarkable-entry-is-in-root-collection? e)
      (not (null (remarkable--find-entry (remarkable-entry-parent e) es)))))


(defun remarkable-entry-name (e)
  "Return the user-level name associated with E."
  (remarkable-entry-metadata-get e :visibleName))


(defun remarkable-entry-is-collection? (e)
  "Check whether E is a collection (folder)."
  (equal (remarkable-entry-metadata-get e :type) "CollectionType"))


(defun remarkable-entry-is-document? (e)
  "Check whether E is a document."
  (equal (remarkable-entry-metadata-get e :type) "DocumentType"))


(defun remarkable-entry-is-deleted? (e)
  "Check whether E has been deleted."
  (equal (remarkable-entry-metadata-get e :parent) "trash"))


(defun remarkable-entry-last-modified (e)
  "Return the last-modified time of entry E as a Lisp timestamp."
  (remarkable--timestamp  (remarkable-entry-metadata-get e :lastModified)))


(defun remarkable-entry-last-opened (e)
  "Return the last-opened time of entry E as a Lisp timestamp."
  (remarkable--timestamp (remarkable-entry-metadata-get e :lastOpened)))


(defun remarkable-entry-contents (e)
  "Return the list of sub-entries of E."
  (plist-get e :contents))


(defun remarkable-entry-has-contents? (e)
  "Test whether E has contents, i.e., is a non-empty collection."
  (not (null (remarkable-entry-contents e))))


(defun remarkable-entry-subfile-entries (e)
  "Return entries for the sub-files of entry E.

These entries aren't stored and are retrieved from the cloud on
demand, and let us interact with the sub-files directly when
necessary."
  (remarkable--get-index (remarkable-entry-hash e)))


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
  (let ((now (remarkable--lisp-timestamp (current-time))))
    (list :visibleName (remarkable--filename-to-name fn)
	  :type "DocumentType"
	  :parent parent
	  :version 0
	  :createdTime "0"
	  :lastModified now
	  :lastOpened ""
	  :lastOpenedPage 0
	  :synced t
	  :pinned :json-false
	  :modified :json-false
	  :deleted :json-false
	  :metadatamodified :json-false)))


(defun remarkable--create-content-plist (fn)
  "Create the content description for a document FN."
  (let ((length (f-length fn))
	(ext (f-ext fn)))
    (list :dummyDocument :json-false
	  :fileType ext
	  :extraMetadata (list :LastBrushColor ""
			       :LastBrushThicknessScale: ""
			       :LastColor ""
			       :LastEraserThicknessScale ""
			       :LastEraserTool ""
			       :LastPen "Finelinerv2"
			       :LastPenColor ""
			       :LastPenThicknessScale ""
			       :LastPencil ""
			       :LastPencilThicknessScale ""
			       :LastTool "Finelinerv2"
			       :ThicknessScale ""
			       :LastFinelinerv2Size "1")
	  :fontName ""
	  :lastOpenedPage 0
	  :lineHeight -1
	  :margins 180
	  :orientation ""
	  :pageCount 0
	  :textScale 1
	  :pages :json-null
	  :redirectionPageMap :json-null
	  :pageTags :json-null
	  :transform (list :m11 1
			   :m12 0
			   :m13 0
			   :m21 0
			   :m22 1
			   :m23 0
			   :m31 0
			   :m32 0
			   :m33 1))))


(defun remarkable--create-pagedata (fn ext)
    "Create the page data for FN with type EXT"
    "\n")


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
  "Upload the subfiles FS and create a document file index."
  (let* ((sorted-files (sort fs #'string<))
	 (hash (remarkable--sha256-files sorted-files))
	 (index (remarkable--create-index fs)))

    ;; upload the subfiles
    (mapc #'remarkable--put-blob sorted-files)

    ;; upload the index for the new document
    (remarkable--put-blob-data index hash)))


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
	 (ext (f-ext fn))
	 (tmp (remarkable--create-temporary-directory-name uuid))
	 (content-fn (f-swap-ext (f-join tmp uuid) ext))
	 (metadata-fn (f-swap-ext (f-join tmp uuid) "metadata"))
	 (metacontent-fn (f-swap-ext (f-join tmp uuid) "content"))
	 (metadata (remarkable--create-metadata-plist fn parent))
	 (metacontent (remarkable--create-content-plist fn)))
    (unwind-protect
	(progn
	  ;; check the parent exists and is a collection
	  (if (not (or (null parent)
		       (equal parent "")))
	      (let ((p (remarkable--find-entry parent remarkable--root-hierarchy)))
		(if (null p)
		    ;; parent doesn't exist
		    (error "Parent %s doesn't exist" parent)

		  (if (not (remarkable-entry-is-collection? p))
		      (error "Parent %s is not a collection" parent)))))

	  ;; set title if supplied
	  (if title
	      (plist-put metadata :visibleName title))

	  ;; create the temporary directory
	  (f-mkdir-full-path tmp)

	  ;; create the metadata files of different kinds
	  (remarkable--create-json-file metadata-fn metadata)
	  (remarkable--create-json-file metacontent-fn metacontent)

	  ;; copy in content file (this makes file name handling easier)
	  (f-copy fn content-fn)

	  ;; create the document entry and add it to the hierarchy
	  (let* ((e (remarkable--create-entry content-fn
					      uuid
					      hash
					      metadata
					      (list metacontent-fn)))
		 (hier (remarkable--add-entry e remarkable--root-hierarchy)))

	    ;; upload the document and its sub-files
	    (remarkable--upload-document-subfiles sorted-files)

	    ;; update the root index and local state
	    (remarkable--upload-root-index hier)

	    ;; return the UUID of the newly-created document
	    uuid))

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
