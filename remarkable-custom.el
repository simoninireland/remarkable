;;; remarkable-custom.el --- ReMarkable customisation -*- lexical-binding: t -*-

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

;; Customisation group and values

;;; Code:


;; ---------- Customisation group ----------

(defgroup Remarkable nil
  "Connecting to the ReMarkable tablet."
  :group External
  :version 1)


;; ---------- Device registration and authentication tokens ----------

(defcustom remarkable-uuid nil
  "The ReMarkable cloud UUID for this client."
  :group Remarkable
  :type 'string)

(defcustom remarkable-device-token nil
  "The ReMarkable cloud device registration token for this client."
  :group Remarkable
  :type 'string)

(defcustom remarkable-user-token nil
  "The ReMarkable cloud user token for this client."
  :group Remarkable
  :type 'string)

(defcustom remarkable-user-token-expires nil
  "The expiry time of the reMarkable cloud user token."
  :group Remarkable
  :type 'number)

(defcustom remarkable-sync-version nil
  "The synchronisation protocol version to use with the ReMarkable cloud."
  :group Remarkable
  :type 'number)


;; ---------- Agent strings ----------

(defcustom remarkable-user-agent "remarkable-emacs"
  "The user agent string used for communicating with the ReMarkable cloud."
  :group Remarkable
  :type 'string)

(defcustom remarkable-device "desktop-linux"
  "The device string used for communncating with the ReMarkable cloud.

This has to be taken from the limited set that the API recognises,
but apparently has no significance."
  :group Remarkable
  :type 'string
  :options )


;; ---------- SHA256 hashes ----------

(defcustom remarkable--sha256-shell-comment "shasum -a 256 -"
  "Shell command used to generate SHA256 hashes.

This should be a filter taking the data to hash on its standard input
and returning the hash on standard output. The actual hash will be
extracted from this output using the regexp given in
`remarkable--sha256-regexp'."
  :group Remarkable
  :type '(choice (const :tag "OS X" "shasum -a 256 -")
		 (const :tag "Linux" "shasum -a 256 -")))


(defcustom remarkable--sha256-regexp  (rx (group (one-or-more (any hex-digit))))
  "Regexp used to extract a SHA256 hash from the output of
`remarkable--sha256-shell-command'."
  :group Remarkable
  :type 'regexp)


;; ---------- File types ----------

(defcustom remarkable--file-types-plist ""
  "Shell command used to generate SHA256 hashes.

This should be a filter taking the data to hash on its standard input
and returning the hash on standard output. The actual hash will be
extracted from this output using the regexp given in
`remarkable--sha256-regexp'."
  :group Remarkable
  :type '(alist :key-type string :value-type function)
  :options '("pdf" "png" "txt"))



(provide 'remarkable-custom)
;;; remarkable-custom.el ends here
