;;; remarkable-cloud-auth.el --- Cloud API authentication -*- lexical-binding: t -*-

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

;; The authentication API.
;;
;; Based heavily on the interfaces provided by:
;;
;;   - https://github.com/juruen/rmapi/
;;
;; The authentication API seems to be stable across changes in the
;; sync API. The version of sync expected is encoded in the JSON
;; Web Token (JWT) used as the user token.
;;
;; One-time device registration code can be obtained from:
;;
;;   - https://my.remarkable.com/device/desktop/connect

;;; Code:

(require 'request)
(require 'json)
(require 'jwt)
(require 'org-id)


;; ---------- Web interface ----------

;; Hosts

(defconst remarkable-auth-host "https://webapp-prod.cloud.remarkable.engineering"
  "Remarkable cloud authentication server.")

;; API endpoints

(defconst remarkable-device-token-url "/token/json/2/device/new"
  "Endpoint for acquiring a ReMarkable cloud device token.")

(defconst remarkable-user-token-url "/token/json/2/user/new"
  "Endpoint for acquiring a ReMarkable cloud user token.")


;; ---------- Local state ----------

(defvar remarkable-user-token nil
  "The ReMarkable cloud user token for this client.

Access using `remarkable-token' rather than directly to ensure
that a new token is acquired when needed, i.e., after expiry.")

(defvar remarkable-user-token-expires nil
  "The expiry time of the ReMarkable cloud user token.

This is parsed from the user token on acquisition.")


;; ---------- Public API ----------

(defun remarkable-authenticate (code)
  "Authenticate against the ReMarkable cloud.

This needs to be run once per client, providing a one-time CODE
that should be obtained from
'https://my.remarkable.com/device/desktop/connect'."
  (interactive "sOne-time code: ")
  (unwind-protect
      (progn
	(message "Authenticating...")

	;; create a UUID for this device
	(unless remarkable-uuid
	  (setq remarkable-uuid (remarkable--uuid)))

	;; get and store the device token
	(let ((dt (remarkable--register-device code)))
	  (setq remarkable-device-token dt))

	(message "Successfully authenticated"))

    (setq remarkable-device-token nil
	  remarkable-user-token nil)
    (message "Failed to authenticate")))


(defun remarkable-deauthenticate ()
  "Forget authentication against the ReMarkable cloud.

Calling this function will require getting another one-time
code and re-authenticating."
  (setq remarkable-device-token nil
	remarkable-user-token nil)
  (message "De-authenticated successfully"))


(defun remarkable-authenticated? ()
  "True if the client is authenticated to the ReMarkable cloud."
  (and remarkable-device-token t))


(defun remarkable-token ()
  "Return the user token needed to authenticate.

If the user token has not been acquired, or has expired, a new one
is obtained."
  (if (or (null remarkable-user-token)
	  (remarkable--user-token-expired?))
      (let ((ut (remarkable--renew-user-token)))
	  (setq remarkable-user-token ut)

	  ;; parse the token
	  (cl-destructuring-bind (exp proto) (remarkable--parse-user-token ut)
	    ;; check the protocol version is supported
	    (if (not (equal proto remarkable-sync-version))
		(error "Unsupported synchonisation protocol version: %s" proto)))

	    ;; set the expiry time
	    (setq remarkable--user-token-expire exp)
	    (message "New user token acquired, expires %s"
		     (format-time-string "%Y-%m-%d %H:%M:%S: exp"))))

  remarkable-user-token)


;; ---------- API interactions ----------

(defun remarkable--user-token-expired? ()
  "True if the user token has expired."
  (time-less-p remarkable-user-token-expires
	       (time-convert (current-time) 1)))


(defun remarkable--register-device (code)
  "Register this application as a device using the one-time CODE.

The one-time code is submitted to the registration API endpoint
(`remarkable-device-token-url' on `remarkable-auth-host'), along
with a device UUID and a device description (held in `remarkable-device').
This returns a device registration token that persistently identifies
the connection from this device to the ReMarkable cloud."
  (let (token)
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
				(setq token data)))
	:error (cl-function (lambda (&key error-thrown &allow-other-keys)
			      (error "Error %s" error-thrown))))

      ;; return the device token
      token)))


(defun remarkable--parse-user-token (jwt)
  "Parse the JWT user token to extract some API information we need.

Specifically, we extract the version of the synchronisation protocol
the cloud expects, and the expiry time of the token.

Returns a list containing the expiry time and protocol version."
  (let* ((es (jwt-decode jwt))
	 (claims (cadr es))
	 (exp (plist-get claims :exp))
	 (scopes (s-split (rx " ") (plist-get claims :scopes)))
	 (sync (if (or (member "sync:fox" scopes)
		       (member "sync:tortoise" scopes)
		       (member "sync:hare" scopes))
		   1.5

		 ;; otherwise return an unknown protocol
		 'unknown)))
    (list exp sync)))


(defun remarkable--renew-user-token ()
  "Renew the user token using the device token.

This submits a \"POST\" request against the user token endpoint
(`remarkable-user-token-url' on `remarkable-auth-host'), passing
the device token as a bearer authorisation token. This returns
a JWT user token which is then used as the bearer token for the
rest of the API."
  (let (token)
    (request (concat remarkable-auth-host remarkable-user-token-url)
      :type "POST"
      :parser #'buffer-string
      :headers (list (cons "User-Agent" remarkable-user-agent)
		     (cons "Authorization" (concat "Bearer " remarkable-device-token)))
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq token data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
			    (error "Error %s" error-thrown)))))

  ;; return the user token
  token)


(provide 'remarkable-cloud-auth)
;;; remarkable-cloud-auth.el ends here
