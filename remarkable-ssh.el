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
;;
;; The API builds a metadata alist for all the documents on the
;; tablet. Optionally it can also build a document tree reflecting
;; the folder structure.

;;; Code:

(require 'f)
(require 'dash)
(require 'json)


;; ---------- Helper functions ----------

(cl-defun remarkable-ssh--tramp-docstore-path (&optional (p ""))
  "Return the Tramp path to a fle P in the document store.

The path is to the root of the document store if P is nil."
  (format "/sshx:%1$s@%2$s:/home/%1$s/%3$s%4$s"
	  remarkable-ssh--user
	  remarkable-ssh--host
	  remarkable-ssh--docstore-path p))


(defun remarkable-ssh--metadata-file-name-for-uuid (uuid)
  "Return the name of the metadata file for the document UUID."
  (f-join (remarkable-ssh--tramp-docstore-path) (format "%s%s" uuid ".metadata")))


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
	 (entries (-filter (lambda (f) (f-ext? f "metadata")) fns))
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

(defun remarkable-ssh--upload-complete ()
  "Signal the end of an uploading.

This re-starts the main xochitl process to refresh the
view of the tablet's documents."
  (let ((default-directory (remarkable-ssh--tramp-docstore-path)))
    (process-file remarkable-ssh--sync-command)))


(provide 'remarkable-ssh)
;; remarkable-ssh.el ends here
