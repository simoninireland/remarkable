;;; remarkable-cloud.el --- ReMarkable cloud API -*- lexical-binding: t -*-

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

;; The API to the ReMarkable cloud.
;;
;; Based heavily on the interfaces provided by:
;;
;;   - https://github.com/juruen/rmapi/
;;   - https://github.com/subutux/rmapy
;;   - https://github.com/splitbrain/ReMarkableAPI/

;;; Code:

(require 'org)
(require 'request)
(require 'json)


;; ---------- Web interface ----------

;; Parameters

(defconst remarkable-rfc3339-nano "%Y-%m-%dT%H:%M:%SZ"
  "Timestamp format.")

(defconst remarkable-user-agent "remarkable-emacs"
  "User agent string for accessing the ReMarkable cloud.")

(defconst remarkable-device "desktop-windows"
  "Device identification for accessing the ReMarkable cloud.")

(defconst remarkable-user-token-lifetime (* 60 60 24)
  "Presumed lifetime for a ReMarkable cloud user token (one day, in seconds).")


;; Hosts
;; (Seem subject to change: these are correct as of Sep2023.)

(defconst remarkable-auth-host "https://webapp-prod.cloud.remarkable.engineering"
  "Remarkable cloud authentication server.")

(defconst remarkable-doc-host "https://document-storage-production-dot-remarkable-production.appspot.com"
  "Remarkable cloud document management server.")

(defconst remarkable-sync-host "https://internal.cloud.remarkable.com"
   "Remarkable cloud synchronisation server.")

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

;; on the document server
(defconst remarkable-metadata-url "/document-storage/json/2/docs"
  "Endpoint for retrieving the metadata for one or all ReMarkable cloud documents.")


;; ---------- Configuration----------

(defvar remarkable-uuid nil
  "UUID for this client.")

(defvar remarkable-device-token nil
  "The device registration token for this client.")

(defvar remarkable-user-token nil
  "The user token for this client.")

(defvar remarkable-user-token-timestamp nil
  "The time the user token was acquired.")


;; ---------- Authentication ----------

(defun remarkable--uuid ()
  "Ensure we have a UUID for this client, generating one if not."
  (or remarkable-uuid
      (let ((uuid (org-id-uuid)))
	(setq remarkable-uuid uuid))))


(defun remarkable--register-device (code)
  "Register this application as a device using the one-time CODE."
  (let* ((uuid (remarkable--uuid))
	 (body (list (cons "code" code)
		     (cons "deviceDesc" remarkable-device)
		     (cons "deviceID" uuid))))
    (request (concat remarkable-auth-host remarkable-device-token-url)
      :type "POST"
      :parser #'buffer-string
      :data (json-encode body)
      :headers (list (cons "Content-Type" "application/json")
		     (cons "User-Agent" remarkable-user-agent))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq remarkable-device-token data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error %s" error-thrown))))))


(defun remarkable--renew-user-token ()
  "Renew the user token using the device token."
  (request (concat remarkable-auth-host remarkable-user-token-url)
    :type "POST"
    :parser #'buffer-string
    :headers (list (cons "User-Agent" remarkable-user-agent)
		   (cons "Authorization" (concat "Bearer " remarkable-device-token)))
    :sync t
    :success (cl-function (lambda (&key data &allow-other-keys)
			    (setq remarkable-user-token data)))
    :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			  (error "Error %s" error-thrown))))
  remarkable-user-token)


(defun remarkable-authenticate (code)
  "Authenticate against the ReMarkable cloud.

The one-time CODE should be obtained from
'https://my.remarkable.com/device/desktop/connect'."
  (interactive "sOne-time code: ")
  (unwind-protect
      (progn
	(remarkable--register-device code)
	(remarkable--renew-user-token)
	(message "Successfully authenticated"))
    (unless (and remarkable-device-token remarkable-user-token)
      (setq remarkable-device-token nil
	    remarkable-user-token nil)
      (nessage "Failed to authenticate"))))


(defun remarkable-deauthenticate ()
  "Forget authentication against the ReMarkable cloud.

Calling this function will require getting another one-time
code and re-authenticating."
  (setq remarkable-device-token nil
	remarkable-user-token nil)
  (message "De-authenticated successfully"))


(defun remarkable--authenticated? ()
  "True if the client is authenticated to the ReMarkable cloud."
  (and remarkable-device-token remarkable-user-token t))


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
  (let (documents)
    (request (concat remarkable-doc-host remarkable-metadata-url)
      :type "GET"
      :parser #'json-read
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " remarkable-user-token))
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
      :type "GET"
      :parser #'buffer-string
      :params (list (cons "doc" doc)
		    (cons "withBlob" "true"))
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " remarkable-user-token))
		     (cons "Accept" "application/json"))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq document data)))
      :error (cl-function (lambda (&key data error-thrown &allow-other-keys)
			    (princ data)
			    (error "Error %s" error-thrown))))
    document))


(provide 'remarkable-cloud)
;; remarkable-cloud.el ends here
