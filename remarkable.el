;;; remarkable.el --- Connect org mode to the ReMarkable tablet -*- lexical-binding: t -*-

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

;; remarkable provides a link between Emacs (and specifically org and org-roam)
;; and the ReMarkable tablet (https://remarkable.com).

;;; Code:

;; JSON Web Token parsing
(require 'jwt)

;; Customisation options
(require 'remarkable-custom)

;; Utility functions
(require 'remarkable-utils)

;; Cloud API
(require 'remarkable-cloud-auth)
(require 'remarkable-cloud-sync15)
;;(require remarkable-ssh)

;; org integration
(require remarkable-org)


(provide 'remarkable)
;;; remarkable.el ends here
