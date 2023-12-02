;;; remarkable-custom.el --- ReMarkable customisation -*- lexical-binding: t -*-

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

;; Customisation group and values.

;;; Code:

(require 'f)


;; ---------- Customisation group ----------

(defgroup Remarkable nil
  "Connecting to the ReMarkable tablet."
  :group 'External
  :version 1)


;; ---------- Cache ----------

(defcustom remarkable--cache-file-name (f-join user-emacs-directory "rm-cache.el")
  "File to store the local reMarkable document cache."
  :group 'Remarkable
  :type 'file)


;; ---------- ssh access ----------

(defcustom remarkable-ssh--docstore-path ".local/share/remarkable/xochitl"
  "Relative path to the document store on the reMarkable tablet."
  :group 'Remarkable
  :type 'string)

(defcustom remarkable-ssh--user "root"
  "User on the reMarkable tablet."
  :group 'Remarkable
  :type 'string)

(defcustom remarkable-ssh--host "remarkable"
  "Name of the reMarkable tablet as seen from the client.

This will typically be an entry in the local DNS. It can also
be an IP address to access the tablet over USB."
  :group 'Remarkable
  :type 'string)


;; ---------- Agent strings ----------

(defcustom remarkable--user-agent "remarkable-emacs"
  "The user agent string used for communicating with the reMarkable cloud."
  :group 'Remarkable
  :type 'string)

(defcustom remarkable--device-description "desktop-linux"
  "The device string used for communncating with the reMarkable cloud.

This has to be taken from the limited set that the API recognises,
but apparently has no significance."
  :group 'Remarkable
  :type 'string)


;; ---------- SHA256 hashes ----------

(defcustom remarkable--sha256-shell-command "shasum -a 256 -"
  "Shell command used to generate SHA256 hashes.

This should be a filter taking the data to hash on its standard input
and returning the hash on standard output. The actual hash will be
extracted from this output using the regexp given in
`remarkable--sha256-regexp'."
  :group 'Remarkable
  :type 'string)


(defcustom remarkable--sha256-regexp  (rx (group (one-or-more (any hex-digit))))
  "Regexp used to extract a SHA256 hash from the output of
`remarkable--sha256-shell-command'."
  :group 'Remarkable
  :type 'regexp)


;; ---------- File types ----------

(defcustom remarkable--file-types-plist '("pdf" nil)
  "Shell command used to generate SHA256 hashes.

This should be a filter taking the data to hash on its standard input
and returning the hash on standard output. The actual hash will be
extracted from this output using the regexp given in
`remarkable--sha256-regexp'."
  :group 'Remarkable
  :type '(plist :key-type string :value-type function)
  :options '("pdf"))



(provide 'remarkable-custom)
;;; remarkable-custom.el ends here
