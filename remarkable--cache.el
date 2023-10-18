;;; remarkable-cache.el --- Document cache -*- lexical-binding: t -*-

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

;; Loading and saving the document cache.

;;; Code:

(require 'cl)


;; ---------- Cache file handling ----------

(defun remarkable--save-cache (fn)
  "Save the document cache to FN.

This saves the root hash, generation, and entry hierarchy as Lisp
data structures."
  (save-excursion
    (find-file fn)

    ;; delete any contents
    (delete-region (point-min) (point-max))

    ;; insert the structures
    ;; sd: pretty-printing isn't needed one everything is working
    (cl-prettyprint `(setq remarkable--hash ,remarkable--hash))
    (cl-prettyprint `(setq remarkable--generaton ,remarkable--generation))
    (cl-prettyprint `(setq remarkable--root-hierarchy ',remarkable--root-hierarchy
			   ))

    (save-buffer)
    (kill-buffer)))

(defun remarkable--load-cache (fn)
  "Load the document cache from FN.

This executes the file, which is clearly a bad idea."
  (load fn))


(provide 'remarkable-cache)
;;; remarkable-cache.el ends here
