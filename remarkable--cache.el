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

(require 'cl)


;; ---------- Cache file handling ----------

(defun remarkable--save-cache (fn)
  "Save the document cache to FN.

This saves the root hash, generation, and entry hierarchy as Lisp
data structures."
  (with-temp-file fn
    (prin1 `(:hash ,remarkable--hash
	     :generation ,remarkable--generation
	     :hierarchy ',remarkable--root-hierarchy)
	   (current-buffer))))


(defun remarkable--load-cache (fn)
  "Load the document cache from FN.

We evaluate the data structure as quoted to avoid executing rogue
code that might be introduced the cache file."
  (let* ((raw-config (with-temp-buffer
		       (insert-file-contents fn)
		       (buffer-string)))
	 (config (car (read-from-string raw-config))))
    (if-let ((hash (plist-get config :hash)))
	(setq remarkable--hash hash)
      (error "Corrupted cache (no hash)"))
    (if-let ((gen (plist-get config :generation)))
	(setq remarkable--generation gen)
      (error "Corrupted cache (no generation)"))
    (if-let ((hier (plist-get config :hierarchy)))
	(setq remarkable--root-hierarchy hier)
      (error "Corrupted cache (no collection hierarchy)"))

    ;; avoid returning the hierarchy
    t))


(provide 'remarkable-cache)
;;; remarkable-cache.el ends here
