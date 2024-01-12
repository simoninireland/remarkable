;;; remarkable-entry.el --- File/folder entries -*- lexical-binding: t -*-

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
;; GNU General Public License for more details.1

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Entries representing files and folders.

;;; Code:

(require 'dash)


;; ---------- Hierarchy construction----------

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
			(progn
			  ;; (insert "deferred\n")

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
			      (error "Problem with the entry hierarchy (circular inclusion?)")

			    ;; otherwise carry on processing, passing in the
			    ;; number of the deferred items for later checking
			    (fold-entries deferred done nil (length deferred))))

		      ;; no deferred entries, we're done
		      done)

		  ;; process the next entry
		  (let ((e (copy-entry (car notdone)))
			(rest (cdr notdone)))
		    (progn
		      ;; (insert (format "do %s\n" (remarkable-entry-name e)))
		      (if (or (remarkable-entry-has-no-metadata? e)
			      (remarkable-entry-is-deleted? e)
			      (remarkable-entry-is-in-root-collection? e))
			  ;; entry is in root collection, move to done
			  ;; deleted entries and those without metadata
			  ;; are treated as being in root
			  (progn
			    ;; (insert "into root\n")
			    (fold-entries rest (append done (list e)) deferred ndeferred))

			;; entry is in some other collection, search for it
			(let ((parent (remarkable-entry-parent e)))
			  ;; look for parent in done
			  (if-let ((p (find-collection parent done)))
			      ;; parent is in done, move to there and continue
			      (progn
				;; (insert (format "add to %s\n" (remarkable-entry-name p)))
				(remarkable--add-entry-to-contents e p)
				(fold-entries rest done deferred ndeferred))

			    ;; parent not yet processed, defer
			    ;; (insert "defer\n")
			    (fold-entries rest done (append deferred (list e)) ndeferred)))))))))

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
  "Create an entry for FN with UUID with hash HASH.

METADATA is the metadata plist that is added directly to the
entry. EXTRAFILES is the set of additional files created
alongside the metadata and the raw content."
  (let ((len (f-length fn)))
    (list :hash hash
	  ;;:type remarkable--index-file-type
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


;; ---------- Field access for common information from entries ----------

(defun remarkable-entry-metadata (e)
  "Return the object-level metedata associated with E."
  (plist-get e :metadata))


(defun remarkable-entry-has-no-metadata? (e)
  "Test whether there is metadata in entry E."
  (null (remarkable-entry-metadata e)))


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


(provide 'remarkable-entry)
;;; remarkable-entry.el ends here
