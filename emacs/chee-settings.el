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

(defun chee-find-repodir (dir)
  "Search DIR upwards until a '.chee' directory is found. Return
the resulting directory (i.e. that has '.chee' as its direct
child)."
  (f-traverse-upwards
   (lambda (p)
     (f-exists? (f-join p ".chee")))
   (expand-file-name dir)))

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
  " %s, %a (%m %o) [%T]"
  "The format string used to format properties line in minibuffer
when navigating thumbnails. The result is appended to the result
of `image-dired-display-properties-format'. Additional specifiers are:

- s  file size
- a  created or lastmodified time
- w  image width
- h  image height
- m  camera make
- o  camera model
- T  chee tags
- C  chee comment"
  :type 'string
  :group 'chee)

(defcustom chee-dired-detail-info-format
  "File: %f (%s, %wx%h)\nCreated: %a\nCamera: %m %o\nTags: %T\nComment: %C"
  "The format string used to format properties when viewing the
original file using image-dired's
`image-dired-display-thumbnail-original-image'. The information
is inserted below the image. Set it to `nil' to disable this
feature.

Format specifiers are:

- f  file name (without path)
- s  file size
- a  created or lastmodified time
- w  image width
- h  image height
- m  camera make
- o  camera model
- T  chee tag list
- C  chee comment"
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
               (setq-default chee-thumb-size newval)
               (setq-default image-dired-thumb-width
                             (string-to-number (match-string 1 newval)))
               (setq-default image-dired-thumb-height
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

(defcustom chee-page-size
  100
  "The number of images to display at once in a buffer. More
  results can be viewed by navigating to the next page."
  :type 'integer
  :group 'chee)

(defcustom chee-default-repository-dir nil
  "A directory to use as chee repository. If this is set it
  becomes the default when searching (instead of searching
  globally). The directory must contain a '.chee' directory which
  is checked via `chee-find-repodir'."
  :type 'directory
  :set (lambda (optname newval)
         (if newval
             (if-let (dir (chee-find-repodir newval))
                 (setq-default chee-default-repository-dir newval)
               (error "Directory doesn't contain a '.chee' dir."))
           (setq-default chee-default-repository-dir nil)))
  :group 'chee)

(provide 'chee-settings)
;;; chee-settings.el ends here
