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

;; The authentication API.
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

;; Constants needed in various places

(defconst remarkable-user-agent "remarkable-emacs"
  "User agent passed to the ReMarkable cloud.")

(defconst remarkable-device "desktop-linux"
  "Device description used when registering a new device.

This has to be taken from the limited set that the API recognises,
but apparently has no significance.")


;; ---------- Configuration ----------

(defvar remarkable-uuid nil
  "UUID for this client.")

(defvar remarkable-device-token nil
  "The device registration token for this client.")

(defvar remarkable-user-token nil
  "The user token for this client.")

(defvar remarkable-user-token-expires nil
  "The expiry time for the user token.")

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
      (message "Failed to authenticate"))))


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


(defun remarkable-token ()
  "Return the user token needed to authenticate.

If the user token has not been acquired, or has expired, a new one
is obtained."
  (if (or (null remarkable-user-token)
	  (remarkable--user-token-expired))
      (remarkable--renew-user-token))
  remarkable-user-token)


;; ---------- Helper functions ----------

(defun remarkable--user-token-expired ()
  "True if the user token has expired."
  (time-less-p remarkable-user-token-expires (current-time)))


(defun remarkable--uuid ()
  "Return a new UUID for a document or folder.

We re-use `org-id-uuid' for UUID generation."
  (org-id-uuid))


;; ---------- API interactions ----------

(defun remarkable--uuid ()
  "Ensure we have a UUID for this client, generating one if not."
  (or remarkable-uuid
      (let ((uuid (org-id-uuid)))
	(setq remarkable-uuid uuid))))


(defun remarkable--register-device (code)
  "Register this application as a device using the one-time CODE.

The one-time code is submitted to the registration API endpoint
(`remarkable-device-token-url' on `remarkable-auth-host'), along
with a device UUID and a device description (held in `remarkable-device').
This returns a device registration token that persistently identifies
the connection from this device to the ReMarkable cloud."
  ;; create a UUID for this device
  (setq remarkable-uuid (remarkable--uuid))

  ;; request a device token using the one-time code
  (let* ((body (list (cons "code" code)
		     (cons "deviceDesc" remarkable-device)
		     (cons "deviceID" remarkable-uuid))))
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
  "Parse the JWT user token to extract some API information we need.

Specifically, we extract the version of the synchronisation protocol
the cloud expects, and the expiry time of the token."
  (let* ((es (jwt-decode jwt))
	 (claims (cadr es)))

    ;; expiry
    (let ((exp (plist-get claims :exp)))
      (setq remarkable-user-token-expires exp))

    ;; sync version
    (let ((scopes (s-split (rx " ") (plist-get claims :scopes))))
      (if (or (member "sync:fox" scopes)
	      (member "sync:tortoise" scopes)
	      (member "sync:hare" scopes))
	  (setq remarkable-sync-version 1.5)

	;; we only support version 1.5 at the moment
	(error "Unsupported synchronisation protocol requested")))))


(defun remarkable--renew-user-token ()
  "Renew the user token using the device token.

This submits a \"POST\" request against the user token endpoint
(`remarkable-user-token-url' on `remarkable-auth-host'), passing
the device token asa bearer authorisation token. This returns
a JWT user token which is then used as thebearer token for the
rest of the API. The user token is automatically parsed by
calling `remarkable--parse-user-token' to extract some information
about the API to be used."
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
