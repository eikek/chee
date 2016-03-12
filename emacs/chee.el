;;; chee.el --- frontend to chee using dired and image-dired

;; Copyright © 2016- Eike Kettner

;; Version: 0.1.0

;; Package-Requires: ((dash "2.12.1") (s "1.10.0"))

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
(require 'chee-dired)
(require 'chee-query)

(defun chee-run-query-dired ()
  "Run the query from the chee-query buffer and display the
results in a dired buffer and thumbnails in
`image-dired-create-thumbnail-buffer'. Switch to the dired
buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (when (and (cdr (window-list))
               (window-live-p (get-buffer-window buf)))
      (delete-window (get-buffer-window buf)))
    (apply 'chee-dired (chee-query-get-args buf))
    (switch-to-buffer (chee-dired-get-buffer))))

(defun chee-run-query-dwim ()
  "Run the query from the chee-query buffer and display the
results in a dired buffer and thumbnails in
`image-dired-create-thumbnail-buffer'. Switch to the dired and
thumbnail buffer."
  (interactive)
  (chee-run-query-dired)
  (pop-to-buffer (image-dired-create-thumbnail-buffer)))

(define-key chee-query-mode-map (kbd "C-c C-c") 'chee-run-query-dwim)
(define-key global-map (kbd "C-c C-s") 'chee-query-open)


(provide 'chee)
;;; chee.el ends here
