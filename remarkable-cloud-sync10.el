;;; remarkable-cloud.el --- Cloud sync 1.0 API -*- lexical-binding: t -*-

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

;; The synchronisation 1.0 API
;;
;; Based heavily on the interfaces provided by:
;;
;;   - https://github.com/juruen/rmapi/

;;; Code:

(require 'org)
(require 'request)
(require 'json)
(require 'jwt)


;; ---------- Web interface ----------

;; Hosts

(defconst remarkable-doc-host "https://document-storage-production-dot-remarkable-production.appspot.com"
  "Remarkable cloud document management server.")

(defconst remarkable-discovery-host "https://service-manager-production-dot-remarkable-production.appspot.com"
  "Discovery service manager for the ReMarkable cloud.")

;; API endpoints

;; on the auth server
(defconst remarkable-device-token-url "/token/json/2/device/new"
  "Endpoint for acquiring a ReMarkable cloud device token.")
(defconst remarkable-user-token-url "/token/json/2/user/new"
  "Endpoint for acquiring a ReMarkable cloud user token.")

;; on the discovery server
(defconst remarkable-discover-storage-url "/service/json/1/document-storage?environment=production&apiVer=2"
  "Endpoint to discover the storage host.")


;; ---------- Public API ----------


;; ---------- Discovery ----------

(defun remarkable--discover-storage-host ()
  "Discover the storage server being used.

This resets the `remarkable-doc-host' to the host returned
from the service manager endpoint."
  (request (concat remarkable-discovery-host remarkable-discover-storage-url)
    :type "GET"
    :parser #'json-read
    :headers (list (cons "User-Agent" remarkable-user-agent)
		   (cons "Accept" "application/json"))
    :sync t
    :success (cl-function (lambda (&key data &allow-other-keys)
			    (let ((host (concat "https://" (cdr (assoc 'Host data)))))
			      (setq remarkable-doc-host host)
			      (message (format "Document storage host reset to %s" host)))))
    :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			  (error "Error %s" error-thrown))))
  remarkable-doc-host)


;; ---------- Document and folder access ----------

(defun remarkable--get-metadata ()
  "Return a list of folders and documents."
  (let ((body (list (cons "http_method" "GET")
		    (cons "relative_path" "root")))
	documents)
    (request (concat remarkable-sync-host remarkable-metadata-url)
      :type "POST"
      :parser #'json-read
      :data (json-encode body)
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " remarkable-user-token))
		     (cons "Content-Type" "application/json")
		     (cons "Accept" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (princ data)
			      (setq documents data)))
      :error (cl-function (lambda (&key data error-thrown &allow-other-keys)
			    (princ data)
			    (error "Error %s" error-thrown))))
    documents))


(defun remarkable--get-document (doc)
  "Return the document identified by DOC."
  (let (document)
    (request (concat remarkable-doc-host remarkable-metadata-url)
      :type "POST"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " remarkable-user-token))
		     (cons "Content-Type" "application/json")
		     (cons "Accept" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq document data)))
      :error (cl-function (lambda (&key data error-thrown &allow-other-keys)
			    (princ data)
			    (error "Error %s" error-thrown))))
    document))


(provide 'remarkable-cloud-sync10)
;; remarkable-cloud-sync10.el ends here
