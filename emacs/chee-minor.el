;;; chee-minor.el --- some utilities

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

;; These are some little utilities using chee from Emacs. It should
;; also support editing chee matadata rec file.

;; The function `chee-minor-display-image-at-point' displays the image
;; if point is on a file checksum (part of).

;; When editing the metadata file it can be used to display the image
;; represented by the current record. The `chee-minor-rec-setup'
;; function sets up some key bindings for rec-mode:

;; - 'v' to `chee-minor-display-image-at-point'
;; - 'V' to toggle a minor mode that displays the image after
;;       navigating to a record.

;;; Code:

(require 'chee-proc)
(require 'thingatpt)
(require 'image-dired)

(defun chee-minor-checksum-at-point ()
  "Get id at point or next word if at 'Checksum'. Nil if not part
of a sha256 checksum."
  (cl-flet ((check (w)
                   (and (> (length w) 3)
                        (string-match-p "[a-f0-9]+" w)
                        w)))
    ;; when in rec-mode, get the key of current record
    (if (fboundp 'rec-log-current-defun)
        (check (rec-log-current-defun))
      (check (thing-at-point 'word)))))

;;;###autoload
(defun chee-minor-view-at-point (id)
  "Run `chee view' to display the image with the given ID. The ID
is a part of the checksum. Interactively, get the current word at
point."
  (interactive (list (chee-minor-checksum-at-point)))
  (when id
    (message "Running chee view …")
    (chee-proc-async-sexp
     (list "view" (format "id:%s" id))
     (lambda (a b)))))

;;;###autoload
(defun chee-minor-display-image-at-point (id)
  "Use the given ID to get the image from chee and use
`image-dired' to display it in another buffer."
  (interactive (list (chee-minor-checksum-at-point)))
  (let ((file (when id
                (message "Running chee …")
                (car (chee-proc-sync-lines
                      (list "find"
                            "--first" "1"
                            "--pattern" "~:path~%"
                            (format "id:%s" id)))))))
    (when file
      (image-dired-display-image file)
      (pop-to-buffer image-dired-display-image-buffer))))


(defun chee-minor-rec-display-next-record ()
  "Call `rec-cmd-goto-next-rec' and display the image afterwards."
  (interactive)
  (rec-cmd-goto-next-rec)
  (let ((win (get-buffer-window)))
    (chee-minor-display-image-at-point (chee-minor-checksum-at-point))
    (select-window win)))

(defun chee-minor-rec-dispaly-prev-record ()
  "Call `rec-cmd-goto-previous-rec' and display the image afterwards."
  (interactive)
  (rec-cmd-goto-previous-rec)
  (let ((win (get-buffer-window)))
    (chee-minor-display-image-at-point (chee-minor-checksum-at-point))
    (select-window win)))

(defun chee-minor-rec-nav-make-map (editing)
  (let ((map (make-sparse-keymap))
        (next (if editing "C-c n" "n"))
        (prev (if editing "C-c p" "p"))
        (display (if editing "C-c v" "v"))
        (toggle (if editing "C-c V" "V")))
    (define-key map (kbd display) 'chee-minor-display-image-at-point)
    (define-key map (kbd next) 'chee-minor-rec-display-next-record)
    (define-key map (kbd prev) 'chee-minor-rec-dispaly-prev-record)
    (define-key map (kbd toggle) 'chee-minor-rec-nav-mode)
    map))

(defvar chee-minor--rec-nav-org-mode-map nil)
(defvar chee-minor--rec-nav-org-edit-map nil)

(define-minor-mode chee-minor-rec-nav-mode
  "Minor mode intended to be used with `rec-mode' when editing
chee metadat file. When active it displays the image after
navigating to a record."
  :init-value nil
  :global nil
  :lighter " χ"
  ;; rec-mode has a nav- and edit-mode and swaps keymaps.
  (if chee-minor-rec-nav-mode
      (progn
        (unless (eq major-mode 'rec-mode)
          (message "This minor mode is only applicable for rec-mode.")
          (chee-minor-rec-nav-mode -1))
        (unless chee-minor--rec-nav-org-mode-map
          (setq chee-minor--rec-nav-org-mode-map
                (copy-keymap rec-mode-map)))
        (let ((map (chee-minor-rec-nav-make-map nil)))
            (set-keymap-parent map chee-minor--rec-nav-org-mode-map)
            (setq rec-mode-map map)
            (unless rec-update-p
              (use-local-map map)))

        (unless chee-minor--rec-nav-org-edit-map
          (setq chee-minor--rec-nav-org-edit-map
                (copy-keymap rec-mode-edit-map)))
        (let ((map (chee-minor-rec-nav-make-map t)))
            (set-keymap-parent map chee-minor--rec-nav-org-edit-map)
            (setq rec-mode-edit-map map)
            (when rec-update-p
              (use-local-map map))))
    (when chee-minor--rec-nav-org-mode-map
      (setq rec-mode-map chee-minor--rec-nav-org-mode-map)
      (unless rec-update-p
        (use-local-map chee-minor--rec-nav-org-mode-map)))
    (when chee-minor--rec-nav-org-edit-map
      (setq rec-mode-edit-map chee-minor--rec-nav-org-edit-map)
      (when rec-update-p
        (use-local-map chee-minor--rec-nav-org-edit-map)))))

(defun chee-minor-rec-mode-hook ()
  (local-set-key (kbd "v") 'chee-minor-display-image-at-point)
  (local-set-key (kbd "V") 'chee-minor-rec-nav-mode))

;;;###autoload
(defun chee-minor-rec-setup ()
  ;; there seems to be a bug in rec-mode: it declares rec-mode-hook
  ;; but runs rec-mode-hooks
  (defvar rec-mode-hooks nil)
  (add-to-list 'rec-mode-hooks 'chee-minor-rec-mode-hook))


(chee-minor-rec-setup)

(provide 'chee-minor)
;;; chee-minor.el ends here
