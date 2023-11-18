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
(require 'dash)
(require 'remarkable-bindat)


;; ---------- Constants ----------

(defconst remarkable-rm6--header "reMarkable .lines file, version=6          "
  "File header for .rm v6 files.")

(defconst remarkable-rm6--tags
  (list :id      #16r0f
	:length4 #16r0c
	:byte8   #16r08
	:byte4   #16r04
	:byte1   #16r01)
  "Tags on data.")

(defconst remarkable-rm6--default-height 1872
  "Default document height.")

(defconst remarkable-rm6--default-width 1404
  "Default document width.")


;; ---------- Types for fields in the .rm file ----------

;; Tags for data
(defun remarkable-rm6--unpack-tag (tag)
  "Unpack a byte TAG into a two-part tag.

The tag is encoded as a plist with keys ':index' and ':type'."
  (let ((index (ash tag -4))
	(type (logand tag #16r0f)))
    (list :index index :type type)))

(bindat-defmacro remarkable-rm6--tag-type ()
  "Type of tags."
  '((v u8)
    :unpack-val (remarkable-rm6--unpack-tag v)))


;; UUIDs
(defun remarkable-rm6--unpack-uuid (bytes)
  "Return a stringified UUID from BYTES."
  (cl-flet* ((stringify-byte (b)
	       "Return B as a two-digit hex string."
	       (format "%02x" b))

	     (stringify-uuid (bytes)
	       "Return a string form of BYTES."
	       (apply #'format "%s%s%s%s-%s%s-%s%s-%s%s-%s%s%s%s%s%s"
		      (mapcar #'stringify-byte bytes))))

    (stringify-uuid bytes)))

(bindat-defmacro uuid ()
  "Type of UUIDs encoded as bytes."
  '((v repeat 16 u8)
    :unpack-val (remarkable-rm6--unpack-uuid v)))


;; Block types
(defconst remarkable-rm6--block-type
  (bindat-type
    (length         uint 32 t)
    (unknown        fill 1)
    (minVersion     u8)
    (currentVersion u8)
    (blockType      u8)
    (data           vec length u8))
  "Type of top-level blocks.")

(defconst remarkable-rm6--blocks-type
  (bindat-type
    (blocks unbounded remarkable-rm6--block-type))
  "Type of a list of top-level blocks.")


;; Sub-block type constructor
(bindat-defmacro remarkable-rm6--subblocks (n type)
  "Type constructor for N sub-blocks of type TYPE within a block."
  `((blockTag remarkable-rm6--tag-type)
    (length   uint 32 t)
    (subBlocks repeat ,n
	       ,type)))


;; Simple types
(defconst remarkable-rm6--id-type
  (bindat-type
    (valtag remarkable-rm6--tag-type)
    (part1 u8)
    (part2 varuint))
  "Type of tagged IDs.")

(defconst remarkable-rm6--boolean-type
  (bindat-type
    (valtag remarkable-rm6--tag-type)
    (value  u8))
  "Type of tagged booleans.")

(defconst remarkable-rm6--byte-type
  (bindat-type
    (valtag remarkable-rm6--tag-type)
    (value  u8))
  "Type of tagged bytes.")

(defconst remarkable-rm6--int-type
  (bindat-type
    (valtag remarkable-rm6--tag-type)
    (value  uint 32 t))
  "Type of tagged integers.")

(defconst remarkable-rm6--single-type
  (bindat-type
    (valtag remarkable-rm6--tag-type)
    (value  single))
  "Type of tagged single-precision floats.")

(defconst remarkable-rm6--double-type
  (bindat-type
    (valtag remarkable-rm6--tag-type)
    (value  double))
  "Type of tagged double-precision floats.")

(defconst reamrkable-rm6--string-value-type
  (bindat-type
    (length  varuint)
    (isASCII u8)
    (value   str length))
  "Type of string values.")

(defconst remarkable-rm6--string-type
  (bindat-type
    (string remarkable-rm6--subblocks 1
	    (value type reamrkable-rm6--string-value-type)))
  "Type of tagged strings.")


;; LWW types
(bindat-defmacro remarkable-rm6--lww-value-type (type)
  "Type constructor for LWW values."
  `((timestamp type remarkable-rm6--id-type)
    (value     type ,type)))

(bindat-defmacro remrkable-rm6--lww-type (type)
  "Type constructor for last-written (timestamped) values of TYPE.

TYPE should be the name of a 'bindat' type."
  `((lww remarkable-rm6--subblocks 1
	 (value remarkable-rm6--lww-value-type ,type))))

(defconst remarkable-rm6--lww-boolean-type
  (bindat-type
    (value remrkable-rm6--lww-type remarkable-rm6--boolean-type))
  "Type of LWW booleans.")

(defconst remarkable-rm6--lww-byte-type
  (bindat-type
    (value remrkable-rm6--lww-type remarkable-rm6--byte-type))
  "Type of LWW bytes.")

(defconst remarkable-rm6--lww-single-type
  (bindat-type
    (value remrkable-rm6--lww-type remarkable-rm6--single-type))
  "Type of LWW single-precision floats.")

(defconst remarkable-rm6--lww-id-type
  (bindat-type
    (value remrkable-rm6--lww-type remarkable-rm6--id-type))
  "Type of LWW IDs.")

(defconst remarkable-rm6--lww-string-type
  (bindat-type
    (value remrkable-rm6--lww-type remarkable-rm6--string-type))
  "Type of LWW strings.")


;; Data block types
(defconst remarkable-rm6--author-type
  (bindat-type
    (length     varuint)
    (authorUUID uuid)
    (authorId   uint 16 t))
  "Type of author identification.")

(defconst remarkable-rm6--author-block-type
  (bindat-type
    (nSubBlocks varuint)
    (authors    remarkable-rm6--subblocks nSubBlocks
		(author type remarkable-rm6--author-type)))
  "Type of an author block.")

(defconst remarkable-rm6--migration-block-type
  (bindat-type
    (migrationId type remarkable-rm6--id-type)
    (isDevice    type remarkable-rm6--boolean-type)
    (unknown     type remarkable-rm6--boolean-type))
  "Type of a migration block.")

(defconst remarkable-rm6--page-info-block-type
  (bindat-type
    (loadsCount  type remarkable-rm6--int-type)
    (mergesCount type remarkable-rm6--int-type)
    (textChars   type remarkable-rm6--int-type)
    (textLines   type remarkable-rm6--int-type)
    (unknown     type remarkable-rm6--int-type))
  "Type of a page information block.")

(defconst remarkable-rm6--scene-tree-parent-type
  (bindat-type
    (parentId type remarkable-rm6--id-type))
  "Type of scene tree parent nodes.")

(defconst remarkable-rm6--scene-tree-block-type
  (bindat-type
    (treeId   type remarkable-rm6--id-type)
    (nodeId   type remarkable-rm6--id-type)
    (isUpdate type remarkable-rm6--boolean-type)
    (parents  remarkable-rm6--subblocks 1
	      (parent type remarkable-rm6--scene-tree-parent-type)))
  "Type of a scene tree block.")

(defconst remarkable-rm6--tree-node-block-type
  (bindat-type
    (nodeId           type remarkable-rm6--id-type)
    (label            type remarkable-rm6--lww-string-type)
    (isVisible        type remarkable-rm6--lww-boolean-type)
    ;;(anchorId         type remarkable-rm6--lww-id-type)
    ;;(anchorType       type remarkable-rm6--lww-byte-type)
    ;;(anchorThreashold type remarkable-rm6--lww-float-type)
    ;;(anchorOriginX    type remarkable-rm6--lww-float-type)
    )
  "Type of a tree node block.")


;; ---------- Public API ----------

;; ---------- Segments, strokes, layers, and pages ----------

;; ---------- .rm parser ----------

(defconst remarkable-rm6--block-type-lookup
  (list (list #16r09 remarkable-rm6--author-block-type)
	(list #16r00 remarkable-rm6--migration-block-type)
	(list #16r0A remarkable-rm6--page-info-block-type)
	(list #16r01 remarkable-rm6--scene-tree-block-type)
	(list #16r02 remarkable-rm6--tree-node-block-type)
	)
  "Mapping from block type bytes to bindat types for parsing.")


(defun remarkable-rm6--read-file (fn)
  "Read .rm file from file FN.

Return the binary data. Signal an error if the file is not in .rm
version 6 format."
  (with-temp-buffer
    (insert-file-contents-literally fn)
    (goto-char 0)
    (if (search-forward remarkable-rm6--header)
	;; search succeeded, return the data
	(string-as-unibyte (buffer-substring (point) (point-max)))

      ;; wrong header
      (error "Wrong header for file"))))


(defun remarkable-rm6--get-block-type (b)
  "Return the rm6 type of block B."
  (bindat-get-field b 'blockType))


(defun remarkable-rm6--get-block-bindat-type (b)
  "Return the bindat type of block B.

This uses the 'blockType' field as a key to find the type.
Return nil if there is no matching block type."
  (let* ((type (remarkable-rm6--get-block-type b))
	 (bt (assoc type
		    remarkable-rm6--block-type-lookup)))
    (if bt
	(cadr bt))))


;; ---------- Debugging support ----------

(defun remarkable-rm6--show-blocks (fn)
  "Read blocks from FN and show them."
  (let* ((data (remarkable-rm6--read-file fn))
	 (blocklist (bindat-unpack remarkable-rm6--blocks-type
				   data))
	 (blocks (bindat-get-field blocklist 'blocks)))
    (cl-flet* ((format-id (id)
		 (format "(%s, %s)"
			 (bindat-get-field id 'part1)
			 (bindat-get-field id 'part2)))
	       (format-boolean (b)
		 (if (bindat-get-field b)
		     "yes"
		   "no"))
	       (format-lww (lww)
		 (format "timestamp %s, value %s"
			 (format-id (bindat-get-field lww 'timestamp))
			 (bindat-get-field lww 'value)))

	       (print-author-subblock (b)
		 (let ((authors (bindat-get-field b 'authors 'subBlocks)))
		   (insert (format "Authors (%s)\n" (length authors)))
		   (dolist (a authors)
		     (insert (format "   %s: %s\n"
				     (bindat-get-field a 'author 'authorId)
				     (bindat-get-field a 'author 'authorUUID))))))

	       (print-migration-subblock (b)
		 (let ((id (bindat-get-field b 'migrationId)))
		   (insert (format "Migration id %s\n"
				   (format-id id)))
		   (insert (format "   is device? %s\n"
				   (format-boolean (bindat-get-field b 'isDevice))))))

	       (print-page-info-subblock (b)
		 (insert (format "Page info chars %s, lines %s\n"
				 (bindat-get-field b 'textChars 'value)
				 (bindat-get-field b 'textLines 'value))))

	       (print-scene-tree-subblock (b)
		 (insert "Scene tree \n")
		 (insert (format "   tree id %s\n"
				 (format-id (bindat-get-field b 'treeId))))
		 (insert (format "   node id %s\n"
				 (format-id (bindat-get-field b 'nodeId))))
		 (insert (format "   is update? %s\n"
				 (format-boolean (bindat-get-field b 'isUpdate))))
		 (insert (format "   parent id %s\n"
				 (format-id (bindat-get-field b 'parents 'subBlocks 0 'parent 'parentId)))))

	       (print-tree-node-subblock (b)
		 (insert "Tree node\n")
		 ;;(insert (format "raw %s\n" b))
		 (insert (format "   nodeId %s\n"
				 (format-id (bindat-get-field b 'nodeId))))
		 (insert (format "   label '%s'\n"
				 (bindat-get-field b 'label 'value 'lww 'subBlocks 0 'value 'value 'string 'subBlocks 0 'value 'value))))

	       (print-block (b)
		 "Print details of B."
		 (let ((type (remarkable-rm6--get-block-bindat-type b)))
		   (if type
		       (let* ((data (bindat-get-field b 'data))
			      (d (bindat-unpack type data)))
			 (pcase (remarkable-rm6--get-block-type b)
			   (#16r09
			    (print-author-subblock d))
			   (#16r00
			    (print-migration-subblock d))
			   (#16r0A
			    (print-page-info-subblock d))
			   (#16r01
			    (print-scene-tree-subblock d))
			   (#16r02
			    (print-tree-node-subblock d))
			   ))))))

      ;; print all the blocks
      (mapc #'print-block blocks)

      ;; avoid returning a huge value
      nil)))


(defun remarkable-rm6--show-raw-blocks (fn)
  "Show the raw parse of the blocks in FN."
  (let* ((data (remarkable-rm6--read-file fn))
	 (blocklist (bindat-unpack remarkable-rm6--blocks-type
				   data))
	 (blocks (bindat-get-field blocklist 'blocks)))
      (mapc (lambda (b)
	      (let* ((type (remarkable-rm6--get-block-bindat-type b))
		     (data (bindat-get-field b 'data))
		     (d (bindat-unpack type data)))
		(cl-prettyprint d)))
	    (cl-subseq blocks 0 7))))


(provide 'remarkable-rm6)
;;; remarkable-rm6.el ends here
