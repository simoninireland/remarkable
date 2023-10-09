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

(defconst remarkable--cotent-length-range-header "x-goog-content-length-range"
  "Content length range header.")

(defconst remarkable--generation-match-header "x-goog-if-generation-match"
  "Generation match header.")


;; Accepted file types

(defconst remarkable-file-types (list "pdf" "epub" "rm")
  "List of acceptable file type extensions.")


;; ---------- Public API ----------

(cl-defun remarkable-ls (&optional uuid)
  "Return a document and folder structure for the given folder UUID.

If UUID is omitted, return the structure for the root folder."
  (let* ((folder (remarkable--get-blob-url uuid))
	 (hash-gen (remarkable--get-hash-generation folder))
	 (index (remarkable--get-index (car hash-gen))))
    index))


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


;; ---------- Doanload API interactions ----------

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
			    (error "Error %s" error-thrown))))
    url))


(defun remarkable--get-hash-generation (url)
  "Retrieve the hash and generation from URL.

The URL should be a blob URL for a folder. This is derefereced with
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
			    (error "Error %s" error-thrown))))
    (cons index gen)))


(defun remarkable--get-index (hash)
  "Return the index of the folder identified by HASH.

The hash should have been acquired by `remarkable--get-hash-generation'
called on the appropriate blob URL. To get the index, the API is
interrogated to get a blob URL by calling `remarkable--get-blob-url'
for the hash. This URL is then dereferenced with a \"GET\" request,
which returns the index of the folder. This is passed to
`remarkable--parse-index' to create a folder structure for the folder."
  (let ((url (remarkable--get-blob-url hash))
	raw-index)
    (request url
      :type "GET"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " (remarkable-token))))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq raw-index data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error %s" error-thrown))))
    (let ((lines (s-lines raw-index)))

      ;; check that the index has the correct schema,
      ;; given as the first line
      (if (not (equal (string-to-number (car lines)) 3))
	  (error "Wrong schema version: %s" (car lines)))

      ;; return the parsed index, generated from the rest
      ;; of the lines
      (remarkable--parse-index (cdr lines)))))


(defun remarkable--parse-index (index)
  "Parse the lines in INDEX as the index of a collection.

INDEX shuld be a list of strings consisting of five fields separated
by colons. The elements are split-out and sanitised slightly into a plist
with elements:

   - ':hash' the hash identifying the file
   - ':type' the file type: \"DocumentTyoe\" for documents or
     \"CollectionType\" for folders
   - ':uuid' the UUID of the elelent
   - ':subfiles' the number of sub-files, and
   - ':length' the length of the element

Other elements may be added by other functions that process the list."
  (cl-flet ((parse-entry (entry)
	      (let ((fields (s-split ":" entry)))
		(cond ((equal (length fields) 5)                   ;; hash
		       (list :hash (nth 0 fields)
			     ;; rename types to match the metadata fields
			     (let ((type (nth 1 fields)))
			       (pcase type
				 ("80000000"
				  "DocumentType")
				 ("0"
				  "CollectionType")
				 (_
				  (error "Unknown type %s" type))))
			     (nth 2 fields)                        ;; UUID
			     (string-to-number (nth 3 fields))     ;; sub-files
			     (string-to-number (nth 4 fields))))   ;; length
		      ((equal entry "")
		       nil)
		      (t
		       (error "Wrong number of fields in index entry: %s" (length fields)))))))
    (mapcar #'parse-entry index)))


;; ---------- Document archives ----------

(defun remarkable--temporary-directory-name ()
  "Create a name for a temporary directory..

Returns the full path to the directory."
  (let ((dir (concat "doc" (number-to-string (random 10000)))))
    (f-join (temporary-file-directory) dir)))


(defun remarkable--create-temporary-directory ()
  "Create a temporary diretory into which to build a document archive.

Returns the full path to the directory."
  (let ((dir (remoarkable--temporary-directory-name)))
    (f-mkdir-full-path dir)
    dir))


;; ---------- Upload API interactions ----------

(defun remarkable--file-type-supported? (fn)
  "Check that the file type of FN is supported.

Supported file type extension are given in `remarkable-file-types'."
  (let ((ext (f-ext fn)))
    (member ext remarkable-file-types)))


(defun remarkable--upload-document (fn uuid)
  "Upload document FN with given UUID."
  (let ((uuid (remarkable--uuid)))



    )
  )



(provide 'remarkable-cloud-sync15)
;; remarkable-cloud-sync15.el ends here
