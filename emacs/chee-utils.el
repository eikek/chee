;;; chee-utils.el --- utility functions

;; Copyright © 2016- Eike Kettner

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'dash)
(require 's)

(defun chee-describe-key (f &optional keymap)
  "Return the key description for the F."
  (let ((keys (where-is-internal f keymap)))
    (s-join ", " (-map 'key-description keys))))

(defun chee-map-index (list f &rest ns)
  "Map F over LIST only applying it on the Nth elements."
  (-map-indexed
   (lambda (index item)
     (if (member index ns)
         (funcall f item index)
       item))
   list))


(provide 'chee-utils)
;;; chee-utils.el ends here
