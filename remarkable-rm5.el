;;; remarkable-rm.el --- .rm document handling -*- lexical-binding: t -*-

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

;; Translate graphics in .rm files into SVG for use in annotations
;; to PDF documents.
;;
;; Based heavily on:
;;
;;   - https://github.com/chemag/maxio/tree/master/rm_tools

;;; Code:

(require 'bindat)
(require 'remarkable-bindat-floats)


;; ---------- Constants ----------

(defconst remarkable-rm--header-regexp
  (rx (seq string-start
	   "reMarkable .lines file, version="
	   (group digit)
	   "          "))
  "Header regexp for .rm files.")

(defconst remarkable-rm--default-height 1872
  "Default document height.")

(defconst remarkable-rm--default-width 1404
  "Default document width.")

(defconst remarkable-rm--stroke-colours
  (list (list 0 0 0)			; black
	(list 255 0 0)			; red
	(list 255,255,255)		; white
	(list 150 0 0)                  ; red-ish
	(list 0 0 125))			; blue
  "Colours for tools, index 0--4.")


;; ---------- Types for fields in the .rm file ----------

(defconst remarkable-rm--header-type
  (bindat-type
    `())
  "Type of the header on a .rm file.")

(defconst remarkable-rm--segment-type
  (bindat-type
    '((x        single t)
      (y        single t)
      (speed    single t)
      (tilt     single t)
      (width    single t)
      (pressure single t)))
  "Type of a segment.")

(defconst remarkable-rm--stroke-v3-type
  (bindat-type
    '((pen       uint 32 t)
      (colour    uint 32 t)
      (fill      uint 4)
      (width     single t)
      (nsegments uint 32 t)
      (segments  repeat (nsegments)
		 (struct remarkable-rm--segment-type))))
  "Type of a stroke in the v3 format.")

(defconst remarkable-rm--stroke-v5-type
  (bindat-type
    '((pen       uint 32 t)
      (colour    uint 32 t)
      (fill      4)
      (width     single t)
      (fill      4)
      (nsegments uint 32 t)
      (segments  repeat (nsegments)
		 (struct remarkable-rm--segment-type))))
  "Type of a stroke in the v5 format.")

(defconst remarkable-rm--layer-v3-type
  (bindat-type
    '((id       uint 32 t)
      (nstrokes uint 32 t)
      (strokes  repeat nstrokes
		(struct remarkable-rm--stroke-v3-type))))
  "Type of a layer in the v3 format.")

(defconst remarkable-rm--layer-v5-type
  (bindat-type
    '((id       uint 32 t)
      (nstrokes uint 32 t)
      (strokes  repeat nstrokes
		(struct remarkable-rm--stroke-v5-type))))
  "Type of a layer in the v5 format.")

(defconst remarkable-rm--v3-type
  (bindat-type
    `((header   str ,(length remarkable-rm--header-v3))
      (nlayers  uint 32 t)
      (layers   repeat nlayers
		(strict remarkable-rm--layer-v3-type))))
  "The v3 file type.")

(defconst remarkable-rm--v5-type
  (bindat-type
    `((header   str ,(length remarkable-rm--header-v5))
      (nlayers  uint 32 t)
      (layers   repeat nlayers
		(strict remarkable-rm--layer-v5-type))))
  "The v5 file type.")


;; ---------- Public API ----------

;; ---------- Segments, strokes, layers, and pages ----------

(defun remarkable-rm--create-segment (id x y speed tilt width pressure)
  "Create a segment starting at (X, Y)."
  (list :id id
	:x x
	:y y
	:speed speed
	:tilt tilt
	:width width
	:pressure pressure))


(defun remarkable-rm--create-stroke (id pen colour width opacity)
  "Create a new stroke.

Strokes consist of segments, drawn with the same tool but with
possible varying speed, tilt, and pressure."
  (list :id id
	:pen pen
	:colour colour
	:width width
	:opacity opacity
	:segments nil))


(defun remarkable-rm--create-layer (id)
  "Create a new layer.

Layers consit of strokes, possbily made with different tools."
  (list :id id
	:strokes nil))


(defun remarkable-rm--create-page ()
  "Create a new page.

Pages consist of layers."
  (list :layers nil))


(defun remarkable-rm--add-segment (stroke segment)
  "Add STROKE to SEGMENT."
  (nconc (plist-get stroke :segments) (list stroke)))


;; ---------- .rm parser ----------

(defun remarkable-rm--load-file (fn)
  "Load the .rm file FN and return its binary contents."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file tmp
      (insert file-contents fn)
      (unibyte-string (buffer-string)))))


(defun remarkable-rm--file-version (s)
  "Extract the file version number from S.

The version number is extracted from the header in the first line."
  (if (string-match remarkable-rm--header-regexp s)
      (number-from-string (match-string 1 s))
    (error "Invalid .rm header")))


(provide 'remarkable-rm)
;;; remarkable-rm.el ends here
