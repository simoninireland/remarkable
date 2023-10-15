;;; remarkable-utils.el --- Utility functions -*- lexical-binding: t -*-

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

;; Utility functions of various kinds.

;;; Code:

;; ---------- Timestamps ----------

(defun remarkable--timestamp (ts)
  "onvert a ReMarkable timestamp to a Lisp timestamp.

The ReMarkable timestamp is measured in mulliseconds since the
epoch. We convert this to the usual Lisp list format for timestamps."
  (let ((s (/ (string-to-number ts) 1000)))
    (decode-time s )))


(defun remarkable--lisp-timestamp (ts)
  "Convert the Lisp time value TS to ReMarkable timestamp format.

The ReMarkable timestamp is measured in mulliseconds since the
epoch."
  (car (time-convert ts 1000)))



;; ---------- Temporary files and directories ----------

(defun remarkable--temporary-directory ()
  "Return the name of the temporary directory to use for files."
  tamporary-file-directory)


(defun remarkable--create-temporary-directory-name (dir)
  "Create a the name of a temporary directory DIR into which to build a document archive.

Returns the full path to the directory."
  (f-join (remarkable--temporary-directory) dir))


;; ---------- SHA256 hashing of files and data ----------

(defconst remarkable--sha256-shell-command "shasum -a 256 -"
  "Shell command used to generate SHA256 hashes.

This should be a filter taking the data to hash on its standard input
and returning the hash on standard output. The actual hash will be
extracted from this output using the regexp given in
`remarkable--sha256-regexp'.")

(defconst remarkable--sha256-regexp (rx (group (one-or-more (any hex-digit))))
  "Regexp used to extract a SHA256 hash from the output of `remarkable--sha256-shell-command'.")


(defun remarkable--sha256-file (fn)
  "Return the SHA256 hash of FN.

This relies on an external program defined in
`remarkable--sha256-shell-command'. The hash is a nunber, but is
returned as a string."
  (with-temp-buffer
    (call-process-shell-command remarkable--sha256-shell-command
				fn
				(current-buffer))
    (goto-char (point-min))
    (if (re-search-forward remarkable--sha256-regexp nil t)
	(match-string 1)
      (error "Failed to generate SHA256 hash for %s" fn))))


(defun remarkable--sha256-files (fns)
  "Return the sum of the hashes of the given files."
  (let* ((hs (mapcar (lambda (fn)
		       (let ((h (remarkable--sha256-file fn)))
			 (string-to-number h 16)))
		     fns)))
    (apply #'+ hs)))


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
	(progn
	  (with-temp-file tmp
	    (insert data))
	  (remarkable--sha256-file tmp))
      (if (f-exists tmp)
	  (f-delete tmp)))))


(provide 'remarkable-utils)
;;; remarkable-utils.el ends here
