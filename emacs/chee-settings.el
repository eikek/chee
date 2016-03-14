;;; chee-settings.el --- vars for chee

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
(require 'dired)
(require 'image-dired)

(defgroup chee nil
  "Interface to the chee program."
  :group 'external)

(defcustom chee-executable
  (executable-find "chee")
  "Name of the chee script to use. If it is not in your $PATH,
specify a full path to the chee program."
  :type '(file :must-match t)
  :safe 'stringp
  :group 'chee)

(defcustom chee-dired-ls-switches
  (or dired-listing-switches "-lh")
  "The switches to ls to use with chee results. If not set, the
value `dired-listing-switches' is used."
  :type 'string
  :group 'chee)

(defcustom chee-dired-properties-format
  " %l, %a (%m %o)"
  "Thet format string used to format properties line in
minibuffer when navigating thumbnails. The result is appended to
the result of `image-dired-display-properties-format'"
  :type 'string
  :group 'chee)

(defcustom chee-thumb-size
  (format "%sx%s"
          (or image-dired-thumb-width image-dired-thumb-size)
          (or image-dired-thumb-height image-dired-thumb-size))
  "The value for the `--size' option passed to the chee
executable.

It is by default set to 'image-dired-thumb-width x
image-dired-thumb-height'. If this value is changed, these two
image-dired properties are updated as well. If you set this
outside of the customize interface, you should take care of
this."
  :type '(string)
  :options '("100x100" "150x150" "200x200" "250x250")
  :set (lambda (optname newval)
         (if (string-match "\\([0-9]+\\)x\\([0-9]+\\)" newval)
             (progn
               (setq chee-thumb-size newval)
               (setq image-dired-thumb-width
                     (string-to-number (match-string 1 newval)))
               (setq image-dired-thumb-height
                     (string-to-number (match-string 2 newval))))
           (error "Wrong value for `chee-thumb-size': %s" newval)))
  :group 'chee)

(defcustom chee-run-function
  'chee-run-query-dwim
  "The function used to run the query. It is bound to `C-c C-c'
in the query window.

Suggested values are

- `chee-run-query-dwim': run the query and split the current
  buffer in two and show the dired and the thumbnail buffer
- `chee-run-query-dired': run the query and switch to the dired
  buffer
- `chee-run-query-thumbnail': run the query and switch to the
  thumbnail buffer

The default is the first option."
  :type 'function
  :options '(chee-run-query-dwim chee-run-query-dired chee-run-query-thumbnail)
  :group 'chee)


(provide 'chee-settings)
;;; chee-settings.el ends here
