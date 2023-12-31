;;; remarkable-cache.el --- Cache management -*- lexical-binding: t -*-

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

;; Loading and saving the collection cache.

;;; Code:

(require 'f)


;; ---------- Public API ----------

(defun remarkable-cache-exists? ()
  "Test if there is a cache file."
  (f-exists? remarkable--cache-file-name))


(defun remarkable-load-cache ()
  "Load the cached state."
  (let ((config (remarkable--load-cache remarkable--cache-file-name)))
    (unless (remarkable--check-cache config)
      (error "Cache seems corrupted"))
    (remarkable--apply-cache config)
    t))


(defun remarkable-save-cache ()
  "Saved the state of the system to the cache.

The old state is backed-up first."
  (remarkable--backup-cache remarkable--cache-file-name)
  (remarkable--save-cache remarkable--cache-file-name)
  t)


(defun remarkable-clear-cache ()
  "Clear all cached data.

This will force a full re-synchronisation at the next call to
`remarkable-sync'.
"
  (if (remarkable-cache-exists?)
      (f-delete remarkable--cache-file-name))
  (setq remarkable--root-hierarchy nil)
  t)


;; ---------- Cache file handling ----------

(defun remarkable--save-cache (fn)
  "Save the document cache to FN.

This persists important state information as a plist:

   - device UUID
   - device token
   - user token
   - user token expiry time
   - root index hash
   - document generation
   - entry hierarchy"
  (with-temp-file fn
    (print `(:device-uuid ,remarkable--device-uuid
	     :device-token ,remarkable--device-token
	     :user-token ,remarkable--user-token
	     :user-token-expires ,remarkable--user-token-expires
	     :hash ,remarkable--hash
	     :generation ,remarkable--generation
	     :hierarchy ,remarkable--root-hierarchy)
	   (current-buffer))))


(defun remarkable--backup-cache (fn)
  "Create a backup of the cache file FN in the same directory.

The backup gets the extension \".bak\"."
  (let ((bfn (f-swap-ext fn "bak")))
    (with-temp-file bfn
      (insert-file-contents fn))))


(defun remarkable--load-cache (fn)
  "Load the document cache from FN.

We evaluate the data structure as quoted to avoid executing rogue
code that might be introduced the cache file.

Returns the cache plist."
  (let* ((raw-config (with-temp-buffer
		       (insert-file-contents fn)
		       (buffer-string))))
    (car (read-from-string raw-config))))


(defun remarkable--check-cache (config)
  "Check the the cache CONFIG contains all the expected elements."
  (-all? (lambda (tag) (plist-get config tag))
		 '(:device-uuid :device-token
		   :user-token :user-token-expires
		   :hash :generation :hierarchy)))


(defun remarkable--apply-cache (config)
  "Apply CONFIG to the global state."
  (if-let ((uuid (plist-get config :device-uuid)))
      (setq remarkable--device-uuid uuid))
  (if-let ((dt (plist-get config :device-token)))
      (setq remarkable--device-token dt))
  (if-let ((ut (plist-get config :user-token)))
      (setq remarkable--user-token ut))
  (if-let ((ute (plist-get config :user-token-expires)))
      (setq remarkable--user-token-expires ute))
  (if-let ((hash (plist-get config :hash)))
      (setq remarkable--hash hash))
  (if-let ((gen (plist-get config :generation)))
      (setq remarkable--generation gen))
  (if-let ((hier (plist-get config :hierarchy)))
      (setq remarkable--root-hierarchy hier)))


(provide 'remarkable-cache)
;;; remarkable-cache.el ends here
