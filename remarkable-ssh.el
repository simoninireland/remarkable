;;; remarkable-ssh.el --- ReMarkable by USB or wifi -*- lexical-binding: t -*-

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

;; An API based on having ssh access to the ReMarkable tablet.
;; This should work both over wifi and over a local USB network.

;;; Code:

(require 'f)
(require 'dash)
(require 'json)


;; ---------- ssh connection ----------

(defclass remarkable-ssh-connection (remarkable-connection)
  ((name-or-ip-address :initarg :target))
  "An ssh-based connection to the reMarkable tablet.

This connection works over wifi (to a tablet identified by its
DNS name or IP address) or over USB to a tablet connected
directly to this computer.

We assume that ssh key access is set-up to the tablet, so that
ssh can be invoked without needing passwords.")


(cl-defmethod remarkable-save ((conn remarkable-ssh-connection))
  (let ((details `(:name-or-ip-address ,(slot-value conn 'name-or-ip-address))))
    (print details (current-buffer))
    (print (slot-value conn 'cache) (current-buffer))))


(cl-defmethod remarkable-load ((conn remarkable-ssh-connection))
  (let ((details (read (current-buffer)))
	(cache (read (current-buffer))))
    (setf (slot-value conn 'name-or-ip-address) (plist-get details :name-or-ip-address))
    (setf (slot-value conn  'cache) cache)))


(cl-defmethod remarkable-index ((conn remarkable-ssh-connection))
  (let ((hier (remarkable-ssh--get-root-index conn)))
    (with-slots (cache) conn
      (setf cache hier)
      (message "Retrieved tablet index")
      cache)))


(cl-defmethod remarkable-sync ((conn remarkable-ssh-connection))
  ;; not yet implemented
 )


(cl-defmethod remarkable-put ((conn remarkable-ssh-connection) fn &key title collection tags)
  (with-slots (cache) conn
    (cl-destructuring-bind (uuid newhier)
	(remarkable-ssh--upload-document conn fn cache :parent collection :title title)
      (setf cache newhier)
      (message "Uploaded document %s" fn)
      uuid)))


;; ---------- Helper functions ----------

(cl-defun remarkable-ssh--tramp-docstore-path (conn &optional (p ""))
  "Return the Tramp path to a file P in the document store of CONN.

The path is to the root of the document store if P is nil."
  (f-join (format "/sshx:%1$s@%2$s:/home/%1$s/%3$s"
		  remarkable-ssh--user
		  (slot-value conn 'name-or-ip-address) ;; used twice in the path
		  remarkable-ssh--docstore-path)
	  p))


(defun remarkable-ssh--metadata-file-name-for-uuid (conn uuid)
  "Return the name of the metadata file for the document UUID on CONN."
  (remarkable-ssh--tramp-docstore-path conn (concat uuid "." remarkable--metadata-ext)))


;; We don't seem to be able to use `f-copy' or `f-mkdir' to operate on
;; the tablet over a Tramp connection, so instead we explicitly spawn
;; a shell process to do the job.
;;
;; Both processes are synchronous. 'call-process' runs locally;
;; `process-file' runs in a remote Tramp-specified directory.

(cl-defun remarkable-ssh--scp-docstore-path (conn &optional (p ""))
  "Return the 'scp' path to a file P in the document store.

This is just the Tramp file path without the leading '/sshx:'."
  (s-replace-regexp (rx (seq bol "/ssh" (? "x") ":")) "" (remarkable-ssh--tramp-docstore-path conn p)))


(defun remarkable-ssh--upload-file (conn fn)
  "Upload FN to the tablet's docstore."
  (let ((tdir (remarkable-ssh--scp-docstore-path conn)))
    (message (format "Copying file %s to tablet %s" (f-filename fn) tdir))
    (call-process "scp" nil nil nil fn tdir)))


(defun remarkable-ssh--download-document (conn uuid tmp)
  "Download all the sub-files of document UUID on CONN to directory TMP."
  (let ((sdir (remarkable-ssh--scp-docstore-path conn uuid)))
    (message (format "Copying files for %s (%s) to %s" uuid sdir tmp))
    (call-process "scp" nil nil nil (format "%s/*" sdir) tmp)))


(defun remarkable-ssh--create-directory (conn dir)
  "Create a directory DIR in the tablet CONN's docstore."
  (let ((default-directory (remarkable-ssh--tramp-docstore-path conn))
	(tdir (f-filename dir)))
    (message (format "Creating tablet directory %s" tdir))
    (process-file "/bin/sh" nil nil nil "-c" (format "mkdir %s" tdir))))


(defun remarkable-ssh--list-document-uuids (conn)
  "List all the document UUIDs on the tablet CONN.

The documents are identified as all the metadata files, and may be
documents or folders."
  (let* ((default-directory (remarkable-ssh--tramp-docstore-path conn))
	 (fns (with-temp-buffer
	       (process-file "/bin/sh" nil t nil "-c" "ls")
	       (butlast (split-string (buffer-string) "\n"))))
	 (metafns (-filter (lambda (fn)
			     (equal (f-ext fn) remarkable--metadata-ext))
			   fns)))
    (mapcar #'file-name-sans-extension metafns)))


;; ---------- Index handling ----------

(defun remarkable-ssh--get-root-index (conn)
  "Return the root index on the tablet connected to with CONN as a hierarchy,"
  (let* ((bare (remarkable-ssh--get-bare-root-index conn))
	 (meta (remarkable-ssh--add-metadata conn bare)))
    (remarkable--make-collection-hierarchy meta)))


(defun remarkable-ssh--get-bare-root-index (conn)
  "Retrieve the bare root index from CONN.

This is just a list of entries containing only the ':uuid'
property, created by finding all the metadata files in the
document store."
  (let* ((uuids (remarkable-ssh--list-document-uuids conn)))
    (mapcar (lambda (uuid)
	      (list :uuid uuid))
	    uuids)))


(defun remarkable-ssh--add-metadata (conn es)
  "Add metadata to the entries in ES from CONN.

This parses the metadata and destructively adds it to the
':metadata' property of each entry in ES."
  (mapc (lambda (e)
	  (let* ((uuid (remarkable-entry-uuid e))
		 (metadata (remarkable-ssh--get-metadata conn uuid)))
	    (plist-put e :metadata metadata)))
	es))


(defun remarkable-ssh--get-metadata (conn uuid)
  "Return the metadata associated with document UUID on CONN.

The metadata is read from the JSON stored in the associated
metadata file, as identified by
`remarkable-ssh--metadata-file-name-for-uuid'."
  (let ((mfn (remarkable-ssh--metadata-file-name-for-uuid conn uuid)))
    (if-let ((raw-metadata (with-temp-buffer
			     (info-insert-file-contents mfn)
			     (buffer-string))))
	(json-parse-string raw-metadata
			   :object-type 'plist)
      (error "Couldn't load metadata from %s" mfn))))


;; ---------- Uploading API ----------

(cl-defun remarkable-ssh--upload-document (conn fn hier &key parent title)
  "Upload document FN to given PARENT on CONN, adding the resulting entry to HIER.

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
	    (remarkable-ssh--create-directory conn uuid)

	    ;; upload the document and its sub-files
	    (mapc (lambda (fn) (remarkable-ssh--upload-file conn fn)) fns)

	    ;; add an entry to the hierarchy
	    (let* ((content-fn (car fns)) ; first sub-file is always the raw contents
		   (other-fns (cddr fns)) ; other sub-files excluding metadata
		   (e (remarkable--create-entry content-fn uuid
						0 ; ignore hashes for now
						metadata
						other-fns))
		   (newhier (remarkable--add-entry e hier)))

	      ;; complete synchronisation
	      (remarkable-ssh--upload-complete conn)

	      ;; return the UUID of the newly-created document
	      ;;and the new hierarchy of entries containing this
	      ;; document
	      (list uuid newhier))))

      ;; clean-up temporary storage
      (if (f-exists? tmp)
	  (f-delete tmp t)))))


(defun remarkable-ssh--upload-complete (conn)
  "Signal the end of uploading to CONN.

This runs the `remarkable-ssh--sync-command' to re-start the
main xochitl UI process and refresh the view of the tablet's
documents."
  (let ((default-directory (remarkable-ssh--tramp-docstore-path conn))
	(parts (s-split " " remarkable-ssh--sync-command)))
    (apply #'process-file (append (list (car parts) nil nil nil) (cdr parts)))))


(provide 'remarkable-ssh)
;; remarkable-ssh.el ends here
