;;; chee-dired.el --- functions to integrate chee with dired and image-dired

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
(require 'chee-proc)
(require 'chee-utils)
(require 'image-dired)
(require 'dash)
(eval-when-compile (require 'cl))

(defvar chee-executable "chee"
  "The chee executable.")

(defvar chee-dired-ls-switches nil
  "The switches to use with ls. Defaults to
  `dired-listing-switches' if nil.")

(defvar chee-dired-buffer-name "*chee-dired*")

(defvar chee-thumb-size (format "%sx%s"
                                (or image-dired-thumb-width image-dired-thumb-size)
                                (or image-dired-thumb-height image-dired-thumb-size))
  "Thumbnail size. Reuse image-dired-thumb-* values.")

(defun chee-dired-get-ls-switches ()
  "Return the switches to use with ls."
  (or chee-dired-ls-switches dired-listing-switches))

(defun chee--thumb-command (query &optional concurrent dir rec first)
  "Create a list to use with `chee-proc-async-sexp'. The command
assembles chee's `thumb' subcommand."
  (append
   (list chee-executable
         "thumb"
         "--size" chee-thumb-size
         "--pattern" "lisp")
   (if concurrent
       (list "--concurrent"))
   (if (stringp dir)
       (list "--file" dir))
   (if rec
       (list "--recursive"))
   (if (numberp first)
       (list "--first" (format "%s" first)))
   (list query)))

(defun chee-dired-get-buffer ()
  (get-buffer-create chee-dired-buffer-name))

(defun chee-dired (query &optional concurrent dir rec first)
  (with-current-buffer (chee-dired-get-buffer)
    (let ((inhibit-read-only t))
      (widen)
      (erase-buffer)
      (with-current-buffer (image-dired-create-thumbnail-buffer)
        (erase-buffer)))
    (kill-all-local-variables)
    (setq default-directory (or dir "/"))
    (setq buffer-read-only t)
    (set (make-local-variable 'dired-sort-inhibit) t)
    (set (make-local-variable 'revert-buffer-function)
         `(lambda (ignore-auto noconfirm)
            (chee-dired ,query ,concurrent ,dir ,rec ,first)))
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons (or dir "/") (point-min-marker))))
    (let* ((pos (point))
           (inhibit-read-only t)
           (ls-switches (chee-dired-get-ls-switches))
           (cmd (chee--thumb-command query concurrent dir rec first)))
      (dired-mode (or dir "/") ls-switches)
      (insert "  " (or dir "/") ":\n")
      (insert "  chee " (mapconcat 'identity cmd " ") "\n")
      (dired-insert-set-properties pos (point))
      (let ((proc (chee-proc-async-sexp cmd (function chee--dired-callback))))
        (chee--setup-kill-key proc (current-buffer))
        (chee--setup-kill-key proc (image-dired-create-thumbnail-buffer))))))

(defun chee--dired-callback (proc plist)
  (-if-let (err (plist-get plist :error-message))
      (message err)
    (let ((original (plist-get plist :origin-path))
          (thumb (plist-get plist :path))
          (buf (get-buffer "*chee-dired*"))
          (inhibit-read-only t)
          (ls-switches (chee-dired-get-ls-switches)))
      (when (and original (buffer-name buf))
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-max))
              (let ((pos (point)))
                (insert (s-trim (shell-command-to-string (concat "ls " ls-switches " " (shell-quote-argument original) "| tail -n -1"))))
                (insert "\n")
                (dired-insert-set-properties pos (point))))))
        (with-current-buffer (image-dired-create-thumbnail-buffer)
          (save-excursion
            (save-restriction
              (goto-char (point-max))
              (let ((beg (point)))
                (image-dired-insert-thumbnail thumb original buf)
                (add-text-properties beg (point) (list :chee-data plist)))
              (cond ((eq 'dynamic image-dired-line-up-method)
                     (image-dired-line-up-dynamic))
                    ((eq 'fixed image-dired-line-up-method)
                     (image-dired-line-up))
                    ((eq 'interactive image-dired-line-up-method)
                     (image-dired-line-up-interactive))
                    ((eq 'none image-dired-line-up-method)
                     nil)
                    (t
                     (image-dired-line-up-dynamic))))))))))

(defun chee--setup-kill-key (proc buf)
  (with-current-buffer buf
    (let ((killfn (lexical-let ((procbuf (process-buffer proc)))
                    (lambda ()
                      (interactive)
                      (with-current-buffer procbuf
                        (message "Killing chee …")
                        (chee-proc-kill)))))
          (map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd "C-c C-k") killfn)
      (use-local-map map))))


(defun chee-dired-make-info-string (plist)
  (format "%s, %s (%s %s)"
          (plist-get plist :origin-created)
          (file-size-human-readable (plist-get plist :origin-length))
          (plist-get plist :origin-make)
          (plist-get plist :origin-model)))

(defun chee-dired--modify-info (origfn &rest args)
  "Advice function to
`image-dired-format-properties-string'. Amend the string with
more info from chee's meta data."
  (let ((res (apply origfn args))
        (plist (get-text-property (point) :chee-data)))
    (if plist
        (concat res " " (chee-dired-make-info-string plist))
      res)))

(advice-add 'image-dired-format-properties-string :around #'chee-dired--modify-info)

(provide 'chee-dired)

;;; chee-dired.el ends here
