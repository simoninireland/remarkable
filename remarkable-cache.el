;;; remarkable-cache.el --- Cache management -*- lexical-binding: t -*-

;; Copyright (c) 2023--2024 Simon Dobson <simoninireland@gmail.com>

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
  (remarkable--load-cache remarkable--cache-file-name)
  t)


(defun remarkable-save-cache ()
  "Saved the state of the system to the cache.

The old state is backed-up first."
  (remarkable--backup-cache remarkable--cache-file-name)
  (remarkable--save-cache remarkable--cache-file-name)
  t)


;; ---------- Cache file handling ----------

(defun remarkable--save-cache (fn)
  "Save the connection to FN.

This persists important connection state information as a plist:

   - the connection type and details
   - the cache of the tablet's contents
   - the associations between org headings and tablet UUIDs

This information is all obtained from the current tablet
connection in `remarkable-tablet'.
"
  (let ((connection `(:connection-type ,(type-of remarkable-tablet))))
    (with-temp-file fn
      (print connection (current-buffer))
      (remarkable-save remarkable-tablet)
      (print remarkable-org--documents (current-buffer)))

    (message "Connection saved to %s" fn)))


(defun remarkable--backup-cache (fn)
  "Create a backup of the cache file FN in the same directory.

The backup gets the extension \".bak\"."
  (let ((bfn (f-swap-ext fn "bak")))
    (with-temp-file bfn
      (insert-file-contents fn))))


(defun remarkable--load-cache (fn)
  "Load the connection from FN.

A new connection of the samme type is created and populated with
the file cache and connection details saved. The associations
between org headings and document UUID are reloaded."
  (with-temp-buffer
    (insert-file-contents fn)
    (goto-char 0)

    ;; read the details
    (let* ((connection (read (current-buffer)))
	   (type (plist-get connection :connection-type))
	   (conn (make-instance type)))

      ;; load the connection authentication details and cache
      (remarkable-load conn)

      ;; read the org associations
      (let ((associations (read (current-buffer))))
	;; install the connection
	(setq remarkable-tablet conn)

	;; install the associations
	(setq remarkable-org--documents associations)

	(message "Connection loaded from %s" fn)))))


(provide 'remarkable-cache)
;;; remarkable-cache.el ends here
