;;; remarkable-rm.el --- .rm v6 document handling -*- lexical-binding: t -*-

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

;; Translate graphics in .rm version 6 files into SVG for use in
;; annotations to PDF documents.
;;
;; Based heavily on:
;;
;;   - https://github.com/ricklupton/rmscene/tree/main
;;   - https://www.reddit.com/r/RemarkableTablet/comments/10hxe3j/updates_regarding_reverse_engineering_remarkable/

;;; Code:

(require 'bindat)
(require 'remarkable-bindat)


;; ---------- Constants ----------

(defconst remarkable-rm6--header "reMarkable .lines file, version=6          "
  "File header for .rm v6 files.")

(defconst remarkable-rm6--tags
  (list :id #16r0f
	:length4 #16r0c
	:byte8 #16r08
	:byte4 #16r04
	:byte1 #16r01)
  "Tags on data.")

(defconst remarkable-rm6--default-height 1872
  "Default document height.")

(defconst remarkable-rm6--default-width 1404
  "Default document width.")


;; ---------- Types for fields in the .rm file ----------

(defun remarkable-rm6--unpack-tag (tag)
  "Unpack a byte TAG into a two-part tag.

The tag is encoded as a plist with keys ':index' and ':type'."
  (let ((index (ash tag -4))
	(type (logand tag #16r0f)))
    (list :index index :type type)))

(bindat-defmacro tag ()
  "Type of tags."
  '((v u8)
    :unpack-val (remarkable-rm6--unpack-tag v)))


(defconst remarkable-rm6--id-type
  (bindat-type
    (valtag tag)
    (part1 u8)
    (part2 varuint))
  "Type of tagged IDs.")

(defconst remarkable-rm6--boolean-type
    (bindat-type
    (valtag tag)
    (value  u8))
  "Type of tagged booleans.")

(defconst remarkable-rm6--byte-type
    (bindat-type
    (valtag tag)
    (value  u8))
  "Type of tagged bytes.")

(defconst remarkable-rm6--int-type
    (bindat-type
    (valtag tag)
    (value  uint 32))
  "Type of tagged integers.")

(defconst remarkable-rm6--single-type
    (bindat-type
    (valtag tag)
    (value  single))
  "Type of tagged single-precision floats.")

(defconst remarkable-rm6--double-type
    (bindat-type
    (valtag tag)
    (value  double))
  "Type of tagged double-precision floats.")

(defconst remarkable-rm6--block-type
  (bindat-type
    (length         uint 32)
    (unknown        fill 1)
    (minVersion     u8)
    (currentVersion u8)
    (blockType      u8))
  "Type of block headers.")

(defconst remarkable-rm6--author-block-type
  (bindat-type
    (nSubBlocks varuint)
    (authors repeat nSubBlocks
	     (struct (length   varuint)
		     (uuid     uint 16)
		     (authorId uint 16))))
  "Type of an author block.")

(defconst remarkable-rm6--migration-block
  (bindat-type
    (migrationId remarkable-rm6--id-type)
    (isDevice    remarkable-rm6--boolean-type)
    (unknown1    remarkable-rm6--boolean-type))
  "Type of a migration block.")


;; ---------- Public API ----------

;; ---------- Segments, strokes, layers, and pages ----------

;; ---------- .rm parser ----------

(defun remarkable-rm6--read-file (fn)
  "Read .rm file from file FN.

Return the binary data. Signal an error if the file is not in .rm
version 6 format."
  (with-temp-buffer
    (insert-file-contents-literally fn)
    (goto-char 0)
    (if (search-forward remarkable-rm6--header)
	;; correct header, return the data
	(let ((after (length remarkable-rm6--header)))
	  (buffer-substring after (point-max)))

      ;; wrong header
      (error "Wrong header for file"))))


(provide 'remarkable-rm6)
;;; remarkable-rm6.el ends here
