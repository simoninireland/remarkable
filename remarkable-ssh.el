;;; remarkable-ssh.el --- ReMarkable by wifi -*- lexical-binding: t -*-

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

;; An API based on having ssh access to the ReMarkable tablet.
;; This should work both over wifi and over a local USB network.
;;
;; The API builds a metadata alist for all the documents on the
;; tablet. Optionally it can also build a document tree reflecting
;; the folder structure.

;;; Code:

(require 'f)
(require 'dash)
(require 'json)


;; ---------- Helper functions ----------

(cl-defun remarkable--tramp-docstore-path (&optional (p ""))
  "Return the Tramp path to a fle P in the document store.

The path is to the root of the document store if P is nil."
  (format "/sshx:%s@%s:%s%s"
	  remarkable--ssh-user
	  remarkable--ssh-host
	  remarkable--ssh-docstore-path p))


;; ---------- Index handling ----------

(defun remarkable-ssh--get-root-index ()
  "Retrieve the root index, returning a hierarchy."
  (cl-flet ((subfiles (uuid)
	      "Return the number of sub-files for document UUID.")))
  (let ((dirs (f-directories remarkable--ssh-docstore-path))))

  )


;; ---------- Uploading API ----------

(defun remarkable-ssh--upload-complete ()
  "Signal the end of an uploading.

This re-starts the main xochitl process to refresh the
view of the tablet's documents."
  (let ((default-directory (remarkable-ssh--tramp-docstore-path)))
    (process-file remarkable-ssh--sync-command)))


(provide 'remarkable-ssh)
;; remarkable-ssh.el ends here
