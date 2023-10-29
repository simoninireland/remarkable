;;; remarkable-utils.el --- Utility functions -*- lexical-binding: t -*-

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

;; Utility functions of various kinds.

;;; Code:

(require 'f)
(require 'json)
(require 'org-id)


;; ---------- Timestamps ----------

(defun remarkable--timestamp (ts)
  "Convert a ReMarkable timestamp to a Lisp timestamp.

The ReMarkable timestamp is measured in mulliseconds since the
epoch. We convert this to the usual Lisp list format for timestamps."
  (let ((s (/ (if (stringp ts)
		  (string-to-number ts)
		ts)
	      1000)))
    (decode-time s )))


(defun remarkable--lisp-timestamp (ts)
  "Convert the Lisp time value TS to ReMarkable timestamp format.

The ReMarkable timestamp is measured in mulliseconds since the
epoch, held as a string."
  (format "%d" (car (time-convert ts 1000))))


;; ---------- UUIDs ----------

(defun remarkable--uuid ()
  "Return a new UUID for a document or folder.

We re-use `org-id-uuid' for UUID generation."
  (org-id-uuid))


;; ---------- Temporary files and directories ----------

(defun remarkable--temporary-directory ()
  "Return the name of the temporary directory to use for files."
  (temporary-file-directory))


(defun remarkable--create-temporary-directory-name (dir)
  "Create a the name of a temporary directory DIR.

Returns the full path to the directory."
  (f-join (remarkable--temporary-directory) dir))


;; ---------- Missing file operations ----------

(defun f-length (fn)
  "Return the length of a file FN in bytes."
  (file-attribute-size (file-attributes fn)))


(defun remarkable--create-json-file (json-fn plist)
  "Write PLIST value to JSON-FN.

The file is pretty-printed when saved, to make it easier to
read (and debug). The keys are alphabetised, since the API seems
to expect this, and empty lists are rendered as empty JSON
objects."
  (let ((json-encoding-object-sort-predicate #'string<)
	(json-null :json-null))
    (let ((json (json-encode plist)))
      (with-temp-file json-fn
	(insert json)
	(json-pretty-print (point-min) (point-max))))))


;; ---------- SHA256 hashing of files and data ----------

(defun remarkable--sha256-file (fn)
  "Return the SHA256 hash of FN.

This relies on an external program defined in
`remarkable--sha256-shell-command'. The hash is a number, but is
returned as a string."
  (with-temp-buffer
    (call-process-shell-command remarkable--sha256-shell-command
				fn
				(current-buffer))
    (goto-char (point-min))
    (if (re-search-forward remarkable--sha256-regexp nil t)
	(match-string 1)
      (error "Failed to generate SHA256 hash for %s" fn))))


(defconst remarkable--sha256-mask (- (ash 1 256) 1)
  "Bitmask to limit a hash code to 256 bits.")


(defun remarkable--sha256-sum (hs)
  "Return the sum of hashes HS.

Since summing the hashes will possibly cause an overflow, we cap
the length of the number at (expt 2 256) and then print to a string
with leading zeros to enxure we have a 64-character hash.

The hash is a number but returned as a hex string."
  (let* ((rawsum (apply #'+ (mapcar (lambda (h) (string-to-number h 16)) hs)))
	 (clippedsum (logand rawsum remarkable--sha256-mask)))
    (format "%064x" clippedsum)))


(defun remarkable--sha256-files (fns)
  "Return the sum of the hashes of the given files.

This simply computes the file hashes and then calls
`remarkable--sha256-sum'. The hash is a number but is
returned as a string."
  (let* ((hs (mapcar #'remarkable--sha256-file fns)))
    (remarkable--sha256-sum hs)))


(defun remarkable--create-temporary-data-file-name ()
  "Create a name based for computing SHA256 hashes of data."
  (let ((stem (format "tmp%s" (random 10000))))
    (f-swap-ext (f-join (remarkable--temporary-directory) stem) "dat")))


(defun remarkable--sha256-data (data)
  "Return the SHA256 hash of the given DATA.

This uses `remarkable--sha256-file' on a temporary file containing
the data."
  (let ((tmp (remarkable--create-temporary-data-file-name)))
    (unwind-protect
	(let ((coding-system-for-write 'no-conversion))
	  (with-temp-file tmp
	    (insert data))
	  (remarkable--sha256-file tmp))
      (if (f-exists? tmp)
	  (f-delete tmp)))))


(provide 'remarkable-utils)
;;; remarkable-utils.el ends here
