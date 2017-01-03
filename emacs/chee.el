;;; chee.el --- Interface to chee using dired and image-dired

;; Copyright © 2016- Eike Kettner

;; Version: 0.2.1-SNAPSHOT
;; URL: https://github.com/eikek/chee/tree/master/emacs
;; Package-Requires: ((dash "2.12.1") (s "1.10.0") (f "0.18.2"))

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

;; This is an interface to the chee program. Chee is a command line
;; tool for managing photos. This package provides a buffer to
;; conveniently execute the chee program. The results are displayed
;; using dired and image-dired.

;; Please see https://github.com/eikek/chee and
;; https://github.com/eikek/chee/tree/master/emacs for more
;; information.

;; Use this package by invoking `chee-query-open'. A few things can be
;; configured, but it is not necessary; see customize-group 'chee' for
;; options. The function `chee-setup-default' adds a key binding for
;; `chee-query-open' to `C-c C-s'.

;;; Code:
(require 'chee-settings)
(require 'chee-dired)
(require 'chee-query)

;;;###autoload
(defun chee-run-query-dired ()
  "Run the query from the chee-query buffer and display the
results in a dired buffer and thumbnails in
`image-dired-create-thumbnail-buffer'. Switch to the dired
buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (apply 'chee-dired (chee-query-get-args buf))
    (when (and (cdr (window-list))
               (window-live-p (get-buffer-window buf)))
      (delete-window (get-buffer-window buf)))
    (switch-to-buffer (chee-dired-get-buffer))))

;;;###autoload
(defun chee-run-query-thumbnail ()
  "Run the query from the chee-query buffer and display the
results in a dired buffer and thumbnails in
`image-dired-create-thumbnail-buffer'. Switch to the thumbnail
buffer."
  (interactive)
  (chee-run-query-dired)
  (switch-to-buffer (image-dired-create-thumbnail-buffer)))

;;;###autoload
(defun chee-run-query-dwim ()
  "Run the query from the chee-query buffer and display the
results in a dired buffer and thumbnails in
`image-dired-create-thumbnail-buffer'. Switch to the dired and
thumbnail buffer."
  (interactive)
  (chee-run-query-dired)
  (pop-to-buffer (image-dired-create-thumbnail-buffer)))

;;;###autoload
(defun chee-setup-default ()
  "Bind the entry point function `chee-query-open' to the key
`C-c C-s' and load `chee-helm' if helm is there. Loading
`chee-helm' replaces some functions with variants using helm."
  (define-key global-map
    (kbd "C-c C-s") 'chee-query-open)
  (if (fboundp 'helm-mode)
      (require 'chee-helm))
  t)


(provide 'chee)
;;; chee.el ends here
