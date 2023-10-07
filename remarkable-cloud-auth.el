;;; remarkable-cloud-auth.el --- Cloud API authentication -*- lexical-binding: t -*-

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

;; The suthentication API.
;;
;; Based heavily on the interfaces provided by:
;;
;;   - https://github.com/juruen/rmapi/
;;
;; The authentication API seems to be stable across changes in the
;; sync API. The version of sync expected is encoded in the JSON
;; Web Token (JWT) used as user token.
;;
;; One-time device registration code can be obtained from:
;;
;;   - https://my.remarkable.com/device/desktop/connect

;;; Code:

(require 'request)
(require 'json)
(require 'jwt)


;; ---------- Web interface ----------

;; Hosts

(defconst remarkable-auth-host "https://webapp-prod.cloud.remarkable.engineering"
  "Remarkable cloud authentication server.")

;; API endpoints

(defconst remarkable-device-token-url "/token/json/2/device/new"
  "Endpoint for acquiring a ReMarkable cloud device token.")
(defconst remarkable-user-token-url "/token/json/2/user/new"
  "Endpoint for acquiring a ReMarkable cloud user token.")


;; ---------- Configuration----------

(defvar remarkable-uuid nil
  "UUID for this client.")

(defvar remarkable-device-token nil
  "The device registration token for this client.")

(defvar remarkable-user-token nil
  "The user token for this client.")

(defvar remarkable-user-id nil
  "The user id for this client..")

(defvar remarkable-sync-version nil
  "The synchronisation protocol version to be used.")


;; ---------- Public API ----------

(defun remarkable-authenticate (code)
  "Authenticate against the ReMarkable cloud.

The one-time CODE should be obtained from
'https://my.remarkable.com/device/desktop/connect'."
  (interactive "sOne-time code: ")
  (unwind-protect
      (progn
	(message "Authenticating...")
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


(defun remarkable-authenticated? ()
  "True if the client is authenticated to the ReMarkable cloud."
  (and remarkable-device-token remarkable-user-token t))


;; ---------- API interactions ----------

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


(defun remarkable--parse-user-token (jwt)
  "Parse the user token JWT to extract some information we need.

Specifically, we extract the version of the synchronisation protocol
the cloud expects, and the user id of the token owner."
  (let* ((es (jwt-decode jwt))
	 (claims (cadr es)))

    ;; user id
    (let* ((auth (cdr (assoc 'auth0-profile claims)))
	   (uid (cdr (assoc 'UserID auth))))
      (setq remarkable-user-id uid))

    ;; sync version
    (let ((scopes (s-split (rx " ") (cdr (assoc 'scopes claims)))))
      (if (or (member "sync:fox" scopes)
	      (member "sync:tortoise" scopes)
	      (member "sync:hare" scopes))
	  (setq remarkable-sync-version 1.5)

	;; we only support version 1.5 at the moment
	(error "Unsupported synchronisation protocol requested")))))


(defun remarkable--renew-user-token ()
  "Renew the user token using the device token."
  (request (concat remarkable-auth-host remarkable-user-token-url)
    :type "POST"
    :parser #'buffer-string
    :headers (list (cons "User-Agent" remarkable-user-agent)
		   (cons "Authorization" (concat "Bearer " remarkable-device-token)))
    :sync t
    :success (cl-function (lambda (&key data &allow-other-keys)
			    (setq remarkable-user-token data)
			    (remarkable--parse-user-token data)))
    :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			  (error "Error %s" error-thrown))))
  remarkable-user-token)


(provide 'remarkable-cloud-auth)
;; remarkable-cloud-auth.el ends here
