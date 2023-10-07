;;; remarkable-files.el --- ReMarkable tablet file handling -*- lexical-binding: t -*-

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

;; The functions to create files for copying to the tablet.

;;; Code:

(require 'f)
(require 'org)


;; ---------- File name patterns ----------

(defconst remarkable--content-ext ".content"
  "Extension for the content file, which mainly holds display metadata.")

(defconst remarkable--metadata-ext ".metadata"
  "Extension for the metadata file.")

(defconst remarkable--pagedata-ext ".pagedata"
  "Extension for the page data file.")

(defconst remarkable--thumbnails-ext ".thumbnails"
  "Extension for the thumbnail image directory.")

(defconst remarkable--localdata-ext ".local"
  "Extension for the local data file.")

(defconst remarkable--pdf-ext ".pdf"
  "Extension for PDF files.")

(defconst remarkable--epub-ext ".epub"
  "Extension for EPUB files.")


;; ---------- Helper functions ----------

(defun remarkable--create-uuid ()
  "Create a new UUID for a document.

We simply re-use the org function `org-id-uuid'."
  (org-id-uuid))


(cl-defun remarkable--timestamp (&optional ts)
  "Return the table timestamp for the time TS.

TS may be in any form accepted by `time-convert'. If omitted,
use the current time."
  (* (time-convert ts 'integer) 1000))


(cl-defun remarkable--file-name (uuid &optional type)
  "Rertun the file name for UUID with given TYPE.

TYPE should be an extension, including the leading '.'.
It can be omitted for file names without extensions."
  (unless type
    (setq type ""))
  (remarkable--tramp-doc-store-path (concat "/"  uuid type)))


;; ---------- Metadata abd other files ----------

(defun remarkable--create-metadata (uuid name)
  "Create metadata for the document UUID named NAME."
  (let* ((now (remarkable--timestamp))
	 (meta (list (cons "type" "DocumentType")
		     (cons "deleted" "false")
		     (cons "lastModified" now)
		     (cons "lastOpened" 0)
		     (cons "lastOpenedPage" 0)
		     (cons "modified" "false")
		     (cons "metadataModified" "false")
		     (cons "parent" "")
		     (cons "pinned" "false")
		     (cons "synced" "false")
		     (cons "version" 0)
		     (cons "visibleName" name)))
	 (fn (remarkable--file-name uuid remarkable--metadata-ext)))
    (with-temp-buffer
      (set-visited-file-name fn)
      (insert (json-encode meta))
      (json-pretty-print-buffer)
      (basic-save-buffer))))


(defun remarkable--create-localdata (uuid)
  "Create local data for the document UUID."
  (let ((fn (remarkable--file-name uuid remarkable--localdata-ext)))
    (with-temp-buffer
      (set-visited-file-name fn)
      (insert "{}")
      (basic-save-buffer))))


(defun remarkable--create-content (uuid)
  "Create content file for the document UUID.

Currently blank."
  (let ((fn (remarkable--file-name uuid remarkable--content-ext)))
    (with-temp-buffer
      (set-visited-file-name fn)
      (basic-save-buffer))))


;; ---------- Create PDF file ----------

(defun remarkable--create-pdf (pdf name)
  "Copy PDF to the tablet under named NAME.

This creates the necessary metadata and other files and directories.
Not all are properly filled-in.

Returns the UUID of the new document."
  (let ((uuid (remarkable--create-uuid)))
    ;; create epty directory
    (f-mkdir-full-path (remarkable--file-name uuid))

    ;; create file
    (f-copy pdf (remarkable--file-name uuid remarkable--pdf-ext))

    ;; create metadata and other files
    (remarkable--create-metadata uuid name)
    (remarkable--create-localdata uuid)
    ;(remarkable--create-content uuid)

    (message (format "Created \"%s\" with UUID %s" name uuid))
    uuid))


(provide 'remarkable-files)
;; remarkable-files.el ends here
