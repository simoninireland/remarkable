;;; remarkable-ssh.el --- ReMarkable by wifi -*- lexical-binding: t -*-

;; Copyrighqt (c) 2023 Simon Dobson <simoninireland@gmail.com>

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
;; This should work both over wifi and over a local USB network.

;;; Code:

(require 'f)
(require 'dash)
(require 'json)


;; ---------- Helper functions ----------

(cl-defun remarkable-ssh--tramp-docstore-path (&optional (p ""))
  "Return the Tramp path to a file P in the document store.

The path is to the root of the document store if P is nil."
  (f-join (format "/sshx:%1$s@%2$s:/home/%1$s/%3$s"
		  remarkable-ssh--user
		  remarkable-ssh--host ;; used twice in the path
		  remarkable-ssh--docstore-path)
	  p))


(defun remarkable-ssh--metadata-file-name-for-uuid (uuid)
  "Return the name of the metadata file for the document UUID."
  (remarkable-ssh--tramp-docstore-path (concat uuid "." remarkable--metadata-ext)))


;; We don't seem to be able to use `f-copy' or `f-mkdir' to operate on
;; the tablet over a Tramp connection, so instead we explicitly spawn
;; a shell process to do the job.
;;
;; Both processes are synchronous. 'call-process' runs locally;
;; `process-file' runs in a remote Tramp-specified directory.

(cl-defun remarkable-ssh--scp-docstore-path (&optional (p ""))
  "Return the 'scp' path to a file P in the document store.

This is just the Tramp file path without the leading '/sshx:'.
The path is to the root of the document store if P is nil."
  (f-join (format "%1$s@%2$s:/home/%1$s/%3$s"
		  remarkable-ssh--user
		  remarkable-ssh--host ;; used twice in the path
		  remarkable-ssh--docstore-path)
	  p))


(defun remarkable-ssh--upload-file (fn)
  "Upload FN to the tablet's docstore."
  (let ((tdir (remarkable-ssh--scp-docstore-path)))
    (message (format "Copying file %s to tablet %s" (f-filename fn) tdir))
    (call-process "scp" nil nil nil fn tdir)))


(defun remarkable-ssh--create-directory (dir)
  "Create a directory DIR in the tablet's docstore."
  (let ((default-directory (remarkable-ssh--tramp-docstore-path))
	(tdir (f-filename dir)))
    (message (format "Creating tablet directory %s" tdir))
    (process-file "/bin/sh" nil nil nil "-c" (format "mkdir %s" tdir))))


;; ---------- Index handling ----------

(defun remarkable-ssh--get-root-index ()
  "Return the root index as a hierarchy,"
  (let* ((bare (remarkable-ssh--get-bare-root-index))
	 (meta (remarkable-ssh--add-metadata bare)))
    (remarkable--make-collection-hierarchy meta)))


(defun remarkable-ssh--get-bare-root-index ()
  "Retrieve the bare root index.

This is just a list of entries containing only the ':uuid'
property, created by finding all the mnetadata files in the
document store."
  (let* ((fns (f-files (remarkable-ssh--tramp-docstore-path "/")))
	 (entries (-filter (lambda (f) (f-ext? f remarkable--metadata-ext)) fns))
	 (uuids (mapcar #'f-base entries)))
    (mapcar (lambda (uuid)
	      (list :uuid uuid))
	    uuids)))


(defun remarkable-ssh--add-metadata (es)
  "Add metadata to the entries in ES.

This parses the metadata and destructively adds it to the
':metadata' property of each entry in ES."
  (mapc (lambda (e)
	  (let* ((uuid (remarkable-entry-uuid e))
		 (metadata (remarkable-ssh--get-metadata uuid)))
	    (plist-put e :metadata metadata)))
	es))


(defun remarkable-ssh--get-metadata (uuid)
  "Return the metadata associated with document UUID.

The metadata is read from the JSON stored in the associated
metadata file, as identified by
`remarkable-ssh--metadata-file-name-for-uuid'."
  (let ((mfn (remarkable-ssh--metadata-file-name-for-uuid uuid)))
    (if-let ((raw-metadata (with-temp-buffer
			     (info-insert-file-contents mfn)
			     (buffer-string))))
	(json-parse-string raw-metadata
			   :object-type 'plist)
      (error "Couldn't load metadata from %s" mfn))))


;; ---------- Uploading API ----------

(cl-defun remarkable-ssh--upload-document (fn hier &key parent title)
  "Upload document FN to given PARENT, adding the resulting entry to HIER.

If PARENT is omitted the document goes to the root collection.
If TITLE is supplied it is used as the visible name for the
document."
  (let* ((uuid (remarkable--uuid))
	 (tmp (remarkable--create-temporary-directory-name uuid)))
    (unwind-protect
	(progn
	  (print uuid)
	  ;; create temporary directory
	  (f-mkdir-full-path tmp)

	  (cl-destructuring-bind (metadata fns)
	      (remarkable--create-subfiles fn uuid tmp
					   :parent parent
					   :title title)

	    ;; create the empty marker directory
	    (remarkable-ssh--create-directory uuid)

	    ;; upload the document and its sub-files
	    (mapc #'remarkable-ssh--upload-file fns)

	    ;; add an entry to the hierarchy
	    (let* ((content-fn (car fns))  ; first sub-file is always the raw contents
		   (other-fns (cddr fns))  ; other sub-files excluding metadata
		   (e (remarkable--create-entry content-fn uuid
						0               ; ignore hashes for now
						metadata
						other-fns))
		   (newhier (remarkable--add-entry e hier)))

	      ;; complete synchronisation
	      (remarkable-ssh--upload-complete)

	      ;; return the UUID of the newly-created document
	      ;;and the new hierarchy of entries containing this
	      ;; document
	      (list uuid newhier))))

      ;; clean-up temporary storage
      (if (f-exists? tmp)
	  (f-delete tmp t)))))


(defun remarkable-ssh--upload-complete ()
  "Signal the end of an uploading.

This runs the `remarkable-ssh--sync-command' to re-start the
main xochitl UI process and refresh the view of the tablet's
documents."
  (let ((default-directory (remarkable-ssh--tramp-docstore-path))
	(parts (s-split " " remarkable-ssh--sync-command)))
    (apply #'process-file (append (list (car parts) nil nil nil) (cdr parts)))))


(provide 'remarkable-ssh)
;; remarkable-ssh.el ends here
