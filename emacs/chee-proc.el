;;; chee-proc.el --- functions for calling chee cli program

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
(require 's)
(require 'f)
(require 'chee-settings)

(defvar chee-proc-buffer-name " *chee-proc*")

(defvar chee-callback)
(defvar chee--proc-error-result)

(defun chee-proc-async-sexp (cmd callback &optional cwd buffer-name)
  "Start a process using CMD, which is a list containing the
arguments to the chee executable. The output is collected and
expected to be s-expressions. The sexps are read and CALLBACK is
invoked with two arguments: the process object and one sexp from
the output.

If chee returns with error, CALLBACK is called once with a plist
containing one property `:error-message' with the output of
chee. On successful return, the plist does not contain this
property."
  (let ((buf (get-buffer-create (or buffer-name chee-proc-buffer-name))))
    (let ((proc (get-buffer-process buf)))
      (when proc
        (if (or (not (eq (process-status proc) 'run))
                (yes-or-no-p "A `chee' process is running. Kill it? "))
            (condition-case nil
                (progn
                  (interrupt-process proc)
                  (sit-for 1)
                  (delete-process proc))
              (error nil))
          (error "Cannot run two chee processes in `%s' at onec" (buffer-name buf)))))
    (let* ((proc)
           (map (make-sparse-keymap)))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (setq default-directory (or (and (stringp cwd) cwd) (f-root)))
        (widen)
        (kill-all-local-variables)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (set-keymap-parent map (current-local-map))
        (define-key map (kbd "C-c C-k") 'chee-proc-kill)
        (use-local-map map)
        (set (make-local-variable 'chee-callback) callback)
        (set (make-local-variable 'chee--proc-error-result) nil)
        (setq proc (apply 'start-process "chee" buf (cons chee-executable cmd)))
        (set-process-filter proc (function chee--sexp-filter))
        (set-process-sentinel proc (function chee--sexp-sentinel))
        (move-marker (process-mark proc) (point))
        (setq mode-line-process '(":%s")))
      proc)))

(defun chee--sexp-filter (proc out)
  "The process-filter function for `chee-proc-async-sexp'."
  (let ((buf (process-buffer proc))
        (going t)
        (inhibit-read-only t))
    (with-current-buffer buf
      ;; stop on error output
      (when (and (eq (marker-position (process-mark proc)) (point-min))
                 (not (string-prefix-p "(" out)))
        (setq chee--proc-error-result t))
      (if chee--proc-error-result
          (insert out)
        (save-excursion
          (save-restriction
            (goto-char (point-max))
            (insert out)
            (while going
              (let* ((pos (marker-position (process-mark proc)))
                     (result (condition-case nil
                                 (read-from-string
                                  (buffer-substring pos (point)))
                               (error nil))))
                (when (car result)
                  (move-marker (process-mark proc) (+ pos (cdr result)))
                  (funcall chee-callback proc (car result)))
                (setq going result)))))))))

(defun chee--sexp-sentinel (proc state)
  "The sentinel function for `chee-proc-async-sexp'."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (with-current-buffer buf
      (when (eq (process-status proc) 'signal)
        (message "chee %s." (s-trim state)))
      (when chee--proc-error-result
        (message "chee returned error")
        (funcall chee-callback proc `(:error-message ,(s-trim (buffer-string)))))
      (save-excursion
        (goto-char (point-max))
        (insert "\nchee " state)
        (delete-char -1)
        (insert " at " (substring (current-time-string) 0 19))
        (delete-process proc)
        (force-mode-line-update)))))

(defun chee-proc-kill ()
  "Kill the `chee' process running in the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (eq (process-status proc) 'run)
         (eq (process-filter proc) (function chee--sexp-filter))
         (condition-case nil
             (delete-process proc)
           (error nil)))))

(defun chee-proc-sync-sexp (cmd &optional cwd)
  "Synchronously execute CMD which is a list of arguments to the
chee executable. Read the output into a list of sexps and return
it."
  (with-temp-buffer
    (setq default-directory (or (and (stringp cwd) cwd) (f-root)))
    (insert "(")
    (let ((rc (apply 'call-process chee-executable nil t nil cmd)))
      (insert ")")
      (car (read-from-string (buffer-substring-no-properties (point-min) (point-max)))))))

(defun chee-proc-sync-lines (cmd &optional cwd)
  "Synchronously execute CMD which is a list of arguments to the
chee executable. The output is split around line separators and
returned as list of strings."
  (with-temp-buffer
    (setq default-directory (or (and (stringp cwd) cwd) (f-root)))
    (let ((rc (apply 'call-process chee-executable nil t nil cmd)))
      (split-string (buffer-string) "\r?\n" t))))


(provide 'chee-proc)
;;; chee-proc.el ends here
