;;; remarkable-org.el --- ReMarkable by wifi -*- lexical-binding: t -*-

;; Copyrighqt (c) 2023 Simon Dobson <simoninireland@gmail.com>

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

;; Interface between org mode and the ReMarkable

;;; Code:

(require 'dash)
(require 'org)
(require 'json)


;; ---------- Access buffer ----------

(defun remarkable--make-buffer (meta)
  "Create a buffer containing details of all the documents in META."
  (let ((buf (generate-new-buffer "*ReMarkable documents*")))
    (with-current-buffer buf
      (dolist (uuid (remarkable--uuids meta :folders nil))
	(insert (remarkable--document-name uuid meta))
	(insert (format " (%s)" (remarkable--document-format uuid meta)))
	(newline)))
    (display-buffer buf)))





(provide 'remarkable-org)
;; remarkable-org.el ends here
