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
(eval-when-compile (require 'cl))
(require 'image-dired)
(require 'dash)
(require 'chee-settings)
(require 'chee-proc)
(require 'chee-utils)

(defvar chee-dired-buffer-name "*chee-dired*")

(defun chee-dired-get-ls-switches ()
  "Return the switches to use with ls."
  (or chee-dired-ls-switches dired-listing-switches))

(defun chee--thumb-command (query &optional concurrent dir rec pagenum decrypt method)
  "Create a list to use with `chee-proc-async-sexp'. The command
assembles chee's `thumb' subcommand."
  (append
   (list "thumb"
         "--size" chee-thumb-size
         "--pattern" "lisp")
   (if concurrent
       (list "--concurrent"))
   (if (stringp dir)
       (list "--file" dir))
   (if rec
       (list "--recursive"))
   (let* ((page (or (and (numberp pagenum) (> pagenum 0) pagenum) 1))
          (skip (* chee-page-size (1- page))))
     (list "--first" (format "%s" chee-page-size) "--skip" (format "%s" skip)))
   (if decrypt
       (list "-d"))
   (cond ((string= "default" method) nil)
         ((stringp method) (list "--method" method))
         (t nil))
   (list query)))

(defun chee-dired-get-buffer ()
  (get-buffer-create chee-dired-buffer-name))

(defvar chee--dired-next-page-fn)
(defvar chee--dired-prev-page-fn)

(defun chee--dired-bind-paging-keys ()
  "Sets up a key map for navigating pages. Must be evaluated
inside `chee-dired' function."
  (let ((lmap (make-sparse-keymap)))
    (set (make-local-variable 'chee--dired-next-page-fn)
         `(lambda () (chee-dired ,query ,concurrent ,dir ,rec ,(1+ (or page 1)) ,decrypt ,method ,repodir)))
    (set (make-local-variable 'chee--dired-prev-page-fn)
         `(lambda () (chee-dired ,query ,concurrent ,dir ,rec ,(1- (or page 2)) ,decrypt ,method ,repodir)))

    (set-keymap-parent lmap (current-local-map))
    (define-key lmap (kbd "M-n") (lambda ()
                                   (interactive)
                                   (funcall chee--dired-next-page-fn)))
    (define-key lmap (kbd "M-p") (lambda ()
                                   (interactive)
                                   (funcall chee--dired-prev-page-fn)))
    (define-key lmap (kbd "D") 'chee-dired-rm)
    (use-local-map lmap)))

(defvar chee-dired--search-dir-recursive)
(defvar chee-dired--search-dir)

(defun chee-dired (query &optional concurrent dir rec page decrypt method repodir)
  (message "Showing page %s" (or page 1))
  (with-current-buffer (chee-dired-get-buffer)
    (let ((inhibit-read-only t))
      (widen)
      (erase-buffer)
      (with-current-buffer (image-dired-create-thumbnail-buffer)
        (chee--dired-bind-paging-keys)
        (erase-buffer)))
    (kill-all-local-variables)
    (setq default-directory (or dir repodir "/"))
    (setq buffer-read-only t)
    (let* ((pos (point))
           (inhibit-read-only t)
           (ls-switches (chee-dired-get-ls-switches))
           (cmd (chee--thumb-command query concurrent dir rec page decrypt method)))
      (dired-mode default-directory ls-switches)
      (set (make-local-variable 'chee-dired--search-dir) dir)
      (set (make-local-variable 'chee-dired--search-dir-recursive) rec)
      (set (make-local-variable 'dired-sort-inhibit) t)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker))))
      (set (make-local-variable 'revert-buffer-function)
           `(lambda (ignore-auto noconfirm)
              (chee-dired ,query ,concurrent ,dir ,rec ,page ,decrypt ,method ,repodir)))
      (chee--dired-bind-paging-keys)
      (insert "  " default-directory ":\n")
      (insert "  chee " (mapconcat 'identity cmd " ") "\n")
      (dired-insert-set-properties pos (point))
      (let ((proc (chee-proc-async-sexp cmd (function chee--dired-callback) repodir)))
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
                (insert "  ")
                (insert (s-trim (shell-command-to-string
                                 (concat "ls "
                                         ls-switches
                                         " "
                                         (shell-quote-argument original) "| tail -n -1"))))
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


(defun chee-dired--format-info (fmt plist)
  (format-spec fmt
               (list (cons ?a (or (plist-get plist :origin-created)
                                  (format-time-string "%Y-%m-%d %H:%M:%S"
                                                      (seconds-to-time
                                                       (/ (plist-get plist :origin-lastmodified) 1000)))))
                     (cons ?s (file-size-human-readable (plist-get plist :origin-length)))
                     (cons ?m (or (plist-get plist :origin-make) ""))
                     (cons ?o (or (plist-get plist :origin-model) ""))
                     (cons ?w (or (plist-get plist :origin-width) ""))
                     (cons ?h (or (plist-get plist :origin-height) ""))
                     (cons ?T (or (plist-get plist :origin-tag) ""))
                     (cons ?C (or (plist-get plist :origin-comment) ""))
                     (cons ?f (or (plist-get plist :origin-filename) "<unkwown-file>")))))

(defun chee-dired--modify-info (origfn &rest args)
  "Advice function to `image-dired-format-properties-string'
adding more format specifiers. You can use additional format
specifiers with `image-dired-display-properties-format':

- s  file size
- a  created or lastmodified time
- m  camera make
- o  camera model
- T  chee tags
- C  chee comment"
  (let ((res (apply origfn args))
        (plist (get-text-property (point) :chee-data)))
    (if plist
        ;; b,f,t,c already used by image-dired-format-properties-string
        (chee-dired--format-info (concat res chee-dired-properties-format) plist)
      res)))

(advice-add 'image-dired-format-properties-string :around #'chee-dired--modify-info)

(defun chee-dired--display-info (origfn &rest args)
  "Advice function to `image-dired-display-thumbnail-original-image'."
  (let ((cheedata (get-text-property (point) :chee-data))
        (origres (apply origfn args)))
    (when (and chee-dired-detail-info-format cheedata)
      (with-current-buffer image-dired-display-image-buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert "\n")
            (insert (chee-dired--format-info chee-dired-detail-info-format cheedata))))))
    origres))

(advice-add 'image-dired-display-thumbnail-original-image :around #'chee-dired--display-info)


(defun chee-dired-rm (&optional arg)
  "Remove current or marked files from disk and index."
  (interactive "P")
  (let ((files (dired-map-over-marks (dired-get-filename) arg)))
    (dired-do-delete arg)
    (let ((lines (chee-proc-sync-lines
                  (append (list "rm" "--index") files)
                  default-directory)))
      (message "%s" (mapconcat 'identity lines "\n")))))


(defun chee-dired--run-on-files (cmd &rest args)
  (let* ((files (dired-map-over-marks (dired-get-filename) nil))
         (query (concat "path~\"" (mapconcat 'identity files ";") "\""))
         (lines (chee-proc-sync-lines (append
                                       (list cmd)
                                       args
                                       (if chee-dired--search-dir-recursive (list "-r"))
                                       (if chee-dired--search-dir (list "-f" chee-dired--search-dir))
                                       (list query))
                 default-directory)))
    (message "%s" (mapconcat 'identity lines "\n"))))

(defun chee-dired--meta-attach (action &optional tags-or-comment)
  (if tags-or-comment
      (chee-dired--run-on-files "meta" "attach" action tags-or-comment)
    (chee-dired--run-on-files "meta" "attach" action)))

(defun chee-dired-add-tags (tags)
  "Add new TAGS to the marked files. TAGS is a comma-separated
list of tag names."
  (interactive (list (completing-read "Tag: " (chee-utils-all-tags))))
  (chee-dired--meta-attach "--add-tags" tags))

(defun chee-dired-tags-of-marked-files ()
  "Find the currently marked files with chee and extract all tags."
  (let* ((files (dired-map-over-marks (dired-get-filename) nil))
         (query (concat "path~\"" (mapconcat 'identity files ";") "\"")))
    (delete-dups
     (-mapcat (lambda (pl)
                (chee-utils--split-tags (plist-get pl :tag)))
              (chee-proc-sync-sexp (list "find" "-p" "lisp" query))))))

(defun chee-dired-remove-tags (tags)
  "Remove TAGS from existing tag list of marked files. TAGS is a
comma-separated list of tag names."
  (interactive (list (completing-read "Remove Tag: " (chee-dired-tags-of-marked-files))))
  (chee-dired--meta-attach "--remove-tags" tags))

(defun chee-dired-set-tags (tags)
  "Sets TAGS overwriting all existing tags of marked files. TAGS
is a comma-separated list of tag names."
  (interactive "MSet tags: ")
  (chee-dired--meta-attach "--tags" tags))

(defun chee-dired-drop-tags ()
  "Drop all tags from marked files."
  (interactive)
  (chee-dired--meta-attach "--drop-tags"))

(defun chee-dired-set-comment (comment)
  "Set a new COMMENT to all marked files."
  (interactive "MComment: ")
  (chee-dired--meta-attach "--comment" comment))

(defun chee-dired-drop-comment ()
  "Remove comment from all marked files."
  (interactive)
  (chee-dired--meta-attach "--drop-comment"))

(provide 'chee-dired)
;;; chee-dired.el ends here
