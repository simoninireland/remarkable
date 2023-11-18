;;; remarkable-bindat.el --- Binary type parsing -*- lexical-binding: t -*-

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

;; bindat types for reading and writing IEEE-754 floating point
;; numbers and varuint-encoded integers.
;;
;; The IEEE-754 standard defines a common schema for representing
;; floating-point numbers, parameterised by the number of bits in the
;; exponent and fractional parts. We encode this schema in a macro,
;; then expand it with the sizes specifiied for the different defined
;; precisions. (The standard also defines half-precision, which I've
;; never encountered and so haven't implemented -- but that's a
;; different parameterisation of the same schema.)
;;
;; There's no point reading numbers at higher precisions as Emacs
;; floats correspond to double-preciison (binary64) numbers.
;;
;; See https://en.wikipedia.org/wiki/IEEE_754

;;; Code:

(require 'bindat)
(require 'cl-lib)


;; ---------- Float parsing ----------

(defun remarkable--encode-bytes (bs le)
  "Convert a list of bytes BS to a number.

The bytes are assumed to be presented big-endian (largest or
leftmost bits first) by default. Setting LE to non-nil reads the
bytes little-endian (smallest or rightmost bits first)."
  (cl-reduce (lambda (v b)
	       (+ (ash v 8) b))
	     bs
	     :from-end le))


(defmacro remarkable--unpack-float (bs exp frac &optional le)
  "Unpack the sequence of bytes BS as an IEEE-754 float.

The bytes are treated as big-endian unless LE is non-nil.
The number has 1 sign bit, EXP exponent bits, and FRAC
fraction bits. Different combinations of these numbers
characterise the different IEEE floating-point precisions."
 (let* ((fmask (1- (ash 1 frac)))
	(emask (1- (ash 1 exp)))
	(bias (1- (ash 1 (1- exp)))))
   `(let* ((n (remarkable--encode-bytes ,bs ,le))
	   (f (logand n ,fmask))
	   (e (logand (ash n ,(- frac)) ,emask))
	   (s (logand (ash n ,(- (+ frac exp))) 1))
	   (rf (if (= f 0)
		   f
		 (1+ (* f ,(expt 2 (- frac)))))))
      (* (if (= s 1) -1.0 1.0)
	 rf
	 (expt 2 (- e ,bias))))))


(defun remarkable--unpack-double (bs le)
  "Unpack a double-precision floating-point number.

This corresponds to the IEEE-754 binary64 (double-precision)
format, which has an 11-bit exponent and a 52-bit fraction. The
encoding of the bytes BS is taken as big-endian unless LE is
non-nil, in which case they are read little-endian."
  (remarkable--unpack-float bs 11 52 le))


(defun remarkable--unpack-single (bs le)
  "Unpack a single-precision floating-point number.

This corresponds to the IEEE-754 binary32 (single-precision)
format, which has an 8-bit exponent and a 23-bit fraction. The
encoding of the bytes BS is taken as big-endian unless LE is
non-nil, in which case they are read little-endian."
  (remarkable--unpack-float bs 8 23 le))


;; ---------- Variable-length unsigned integer parsing ----------

;; This seems like a really, /really/ stupid way to encode
;; integers: do we /really/ care about minimising representation
;; sizes for this application?

(defun remarkable--unpack-varuint ()
  "Unpack a variable-length unsigned integer.

The integer is held little-endian packed into 7-bit chunks."

  ;; note that this refers to bindat-raw and bindat-idx taken from the
  ;; dynamic scope
  (let ((i 0)
	(shift 0)
	(result 0))
    (while (let ((b (aref bindat-raw (+ bindat-idx i))))
	     (setf result (logior result
				  (ash (logand b #16r7f) shift)))
	     (cl-incf shift 7)
	     (cl-incf i)
	     (not (logand b #16r80))))

    ;; skip over the bytes we consumed
    (cl-incf bindat-idx i)

    ;; return the int
    result))


;; ---------- Variable-length list repeat ----------

;; Again, we need this because the file format doesn't state
;; how many blocks it contains or introduce a proper termination
;; marker, which makes life harder.

(defun remarkable--unpack-unbounded (type)
  "Unpack a list of TYPE until there are no more to unpack."

  ;; note that this refers to bindat-raw and bindat-idx taken from the
  ;; dynamic scope
  (let ((elements '()))
    (while (and (> (length bindat-raw)
		   bindat-idx)
		(if-let ((next (bindat-unpack type bindat-raw bindat-idx)))
		    (progn
		      (setf elements (append elements (list next)))
		      (cl-incf bindat-idx (bindat-length type next))
		      t))))
    elements))


;; ---------- bindat types ----------

(bindat-defmacro double (&optional le)
  "A double-precision floating-point number.

This type corresponds to the IEEE-754 binary64 type, which is the
native precision of floats in Emacs. if LE is non-nil the number
is read as little-endian; otherwise it is read as big-endian."
  `((bs repeat 8 byte)
    :unpack-val (remarkable--unpack-double bs ,le)))


(bindat-defmacro single (&optional le)
  "A single-precision floating-point number.

This type corresponds to the IEEE-754 binary32 type. if LE is
non-nil the number is read as little-endian; otherwise it is read
as big-endian."
  `((bs repeat 4 byte)
    :unpack-val (remarkable--unpack-single bs ,le)))


(bindat-defmacro varuint ()
  "A variable-length unsigned integer."
  '((v unit 0)
    :unpack-val (remarkable--unpack-varuint)))


(bindat-defmacro unbounded (type)
  "A variable-length list of type TYPE."
  `((l unit 0)
    :unpack-val (remarkable--unpack-unbounded ,type)))


(provide 'remarkable-bindat)
;;; remarkable-bindat.el ends here
