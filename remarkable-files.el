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


;; ---------- File name patterns ----------

(defconst remarkable--content-ext "content"
  "Extension for the content file, which mainly holds display metadata.")

(defconst remarkable--metadata-ext "metadata"
  "Extension for the metadata file.")

(defconst remarkable--pagedata-ext "pagedata"
  "Extension for the page data file.")

(defconst remarkable--thumbnails-ext "thumbnails"
  "Extension for the thumbnail image directory.")

(defconst remarkable--localdata-ext "local"
  "Extension for the local data file.")

(defconst remarkable--pdf-ext "pdf"
  "Extension for PDF files.")

(defconst remarkable--epub-ext "epub"
  "Extension for EPUB files.")


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


(cl-defun remarkable--create-subfiles (fn uuid dir &key parent title)
  "Create the sub-files for file FN in DIR with the given UUID.

The metadata can be altered using PARENT and TITLE.

The file creation is all client-side, to be uploaded later.

Return the metadata and a list of sub-files consisting of the content
sub-file, metadata sub-file, and any others created, in that order."
  ;; check the parent exists and is a collection
  (if (not (or (null parent)
	       (equal parent "")))
      (let ((p (remarkable--find-entry parent remarkable--root-hierarchy)))
	(if (null p)
	    ;; parent doesn't exist
	    (error "Parent %s doesn't exist" parent)

	  (if (not (remarkable-entry-is-collection? p))
	      (error "Parent %s is not a collection" parent)))))

  ;; create metadata and content sub-files
  (let* ((ext (f-ext fn))
	 (content-fn (f-swap-ext (f-join dir uuid) ext))
	 (metadata-fn (f-swap-ext (f-join dir uuid) remarkable--metadata-ext))
	 (metacontent-fn (f-swap-ext (f-join dir uuid) remarkable--content-ext))
	 (metadata (remarkable--create-metadata-plist fn parent))
	 (metacontent (remarkable--create-content-plist fn)))

    ;; set title if supplied
    (if title
	(plist-put metadata :visibleName title))

    ;; create the metadata files of different kinds
    (remarkable--create-json-file metadata-fn metadata)
    (remarkable--create-json-file metacontent-fn metacontent)

    ;; copy in content file (this makes file name handling easier)
    (f-copy fn content-fn)

    ;; return the metadata and the sub-files
    (list metadata
	  (list content-fn metadata-fn metacontent-fn))))


(provide 'remarkable-files)
;; remarkable-files.el ends here
