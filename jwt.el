;;; jwt.el --- JON Web Token functions -*- lexical-binding: t -*-

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

;; Functions for parsing JSON Web Tokens (JWT)
;;
;; See https://datatracker.ietf.org/doc/html/rfc7519 for the standard.

;;; Code:

(require 's)
(require 'json)


;; ---------- Public API ----------

(defun jwt-decode (jwt)
  "Decode the JWT token, returning a list.

The list consists of:

   - A list representation of the JOSE header
   - A list representation of the JWT claims
   - A signature block

The signature currently isn't checked."
  (let* ((js (jwt--split jwt))
	 (jose (json-parse-string (jwt--base64-decode-string (car js))
				  :object-type 'alist))
	 (claims (json-parse-string (jwt--base64-decode-string (cadr js))
				    :object-type 'alist))
	 (sig (caddr js)))
    (list jose claims sig)))


;; ---------- Parsing and verification ----------

(defun jwt--split (jwt)
  "Split the JWT string into a list.

The list consists of the JOSE header, the claims, and the signature,"
  (s-split (rx ".") jwt))


(defun jwt--base64-decode-string (s)
  "Decode a Base64-encoded string that has had trailing '=' characters deleted.

JWT allows padding characters to be deleted. They need to be added
back before decoding using `base64-decode-string'. The padding
pads-out the string so its length is divisible by 4.

See https://en.wikipedia.org/wiki/Base64 for details of Base64."
  (let* ((lm4 (mod (length s) 4))
	 (p (- 4 lm4))
	 (padded (concat s (make-string p ?=))))
    (base64-decode-string padded)))


(provide 'jwt)
;; jwt.el ends here
