;;; chee-query.el --- the query window

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
(require 'chee-settings)
(require 'chee-utils)
(require 'chee-proc)

(defvar chee-query-buffer-name "*chee-query*"
  "The name for the query buffer.")

(defvar chee-query-identifiers
  '("checksum" "location" "path" "filename"
    "extension" "length" "lastmodified" "added"
    "mimetype" "created" "make" "model" "width"
    "height" "orientation" "iso" "date" "id"
    "pixel" "collection")
  "Property identifiers used for syntax highlighting and
  completion.")

(defvar chee-query-comparators
  '(">" "<" "=" ":" "~" "/")
  "Operator characters used for syntax highlighting.")

(defconst chee-query-font-lock-keywords
  (let ((idents (regexp-opt chee-query-identifiers))
        (comps (regexp-opt chee-query-comparators)))
    `(("^#.*" . font-lock-comment-face)
      ("'[^']*'" . font-lock-string-face)
      (,idents . font-lock-keyword-face)
      (,comps . font-lock-builtin-face))))

(defvar chee-query-syntax-table
  (let ((st (make-syntax-table)))
    st))


(defun chee--query-complete-ident (do-complete candidates)
  "Complete the ident at point. If there is no match, call
DO-COMPLETE with the current thing-at-point to do the interactive
completion. CANDIDATES is a list of possible candidates."
  (let* ((end (point))
         (meat (or (thing-at-point 'symbol) ""))
         (maxMatchResult  (try-completion meat candidates)))
    (cond ((eq maxMatchResult t))
          ((null maxMatchResult)
           (message "Can't find completion for “%s”" meat))
          ((not (string= meat maxMatchResult))
           (delete-region (- end (length meat)) end)
           (insert maxMatchResult))
          (t (funcall do-complete meat)))))

(defun chee-query-simple-complete-ident ()
  "Perform ident completion on word before cursor."
  (interactive)
  (chee--query-complete-ident
   (lambda (meat)
     (with-output-to-temp-buffer "*Completions*"
       (display-completion-list
        (all-completions meat chee-query-identifiers))
       meat))
   chee-query-identifiers))

(define-derived-mode chee-query-mode
  prog-mode "chee-query"
  "Major mode for editing chee queries.
\\{chee-query-mode-map}"

  (set-syntax-table chee-query-syntax-table)
  (setq font-lock-defaults '((chee-query-font-lock-keywords)))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (chee-query-set-args "" t nil nil nil)
  (goto-char (point-max)))

(defun chee-query-get-buffer (&optional buffer-or-name)
  "Get or create the buffer holding the query. The buffer is put
in `chee-query-mode' if not already. BUFFER-OR-NAME is used if
specified, otherwise `chee-query-buffer-name' is used."
  (let ((buf (get-buffer-create (or buffer-or-name chee-query-buffer-name))))
    (with-current-buffer buf
      (unless (eq major-mode 'chee-query-mode)
        (chee-query-mode)))
    buf))

(defun chee-query-set-args (query concurrent dir recursive first &optional buffer-or-name)
  "Render QUERY, CONCURRENT, DIR, RECURSIVE and FIRST into the
query buffer. The buffer BUFFER-OR-NAME is used if
specified. Point is restored if it is still valid after the new
values have been inserted."
  (with-current-buffer (chee-query-get-buffer buffer-or-name)
    (let ((inhibit-read-only t)
          (pos (point))
          (makeflag (lambda (flag name)
                      (concat (if flag "[X]" "[ ]") " " name))))
      (erase-buffer)
      (insert "# File: " (or dir "<index>"))
      (insert " (" (chee-describe-key 'chee-query-toggle-file) ")\n")
      (insert "# ")
      (insert (funcall makeflag concurrent "--concurrent"))
      (insert " (" (chee-describe-key 'chee-query-toggle-concurrent) ")")
      (insert "   ")
      (insert (funcall makeflag recursive "--recursive"))
      (insert " (" (chee-describe-key 'chee-query-toggle-recursive) ")")
      (insert "   ")
      (insert "[" (if (numberp first) (format "%3d" first) "   ") "]")
      (insert " --first")
      (insert " (" (chee-describe-key 'chee-query-increment-limit) ")")
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'read-only t)
      (insert "\n")
      (insert query)
      (when (< pos (point-max))
        (goto-char pos)))))

(defun chee-query-get-args (&optional buffer)
  "Return the values in the query buffer as a list '(list query
concurrent dir recursive first)."
  (let (query dir concurrent recursive first)
    (with-current-buffer (chee-query-get-buffer buffer)
      (save-excursion
        (goto-char (point-min))
        (search-forward "File: ")
        (unless (looking-at-p "<index>")
          (setq dir (s-trim (buffer-substring-no-properties
                             (point)
                             (- (line-end-position) (+ 2 (length (chee-describe-key 'chee-query-toggle-file))))))))
        (search-forward-regexp "\\[\\(X\\| \\)\\]")
        (setq concurrent (string= (match-string-no-properties 1) "X"))
        (search-forward-regexp "\\[\\(X\\| \\)\\]")
        (setq recursive (string= (match-string-no-properties 1) "X"))
        (search-forward-regexp "\\[\\([0-9 ]+\\)\\]")
        (let ((num (s-trim (match-string-no-properties 1))))
          (unless (s-blank? num)
            (setq first (string-to-number num))))
        (search-forward "\n")
        (setq query (buffer-substring-no-properties (point) (point-max)))
        (list query concurrent dir recursive first)))))

(defun chee--query-set-arg (f &rest ns)
  (when (eq major-mode 'chee-query-mode)
    (let ((args (chee-query-get-args)))
      (apply 'chee-query-set-args
             (apply 'chee-map-index args f ns)))))

(defun chee-query-toggle-concurrent ()
  "Toggle the concurrent flag."
  (interactive)
  (chee--query-set-arg (lambda (e i) (not e)) 1))

(defun chee-query-toggle-recursive ()
  "Toggle the recursive flag. This flag is only used if a
directory is specififed."
  (interactive)
  (chee--query-set-arg (lambda (e i) (not e)) 3))

(defun chee-query-increment-limit (arg)
  "Incremen the limit parameter. It is incremented by 10. With
prefix argument, it is decremented by `(* arg 10)'."
  (interactive "p")
  (let* ((step (if (> arg 1) (* arg -10) 10))
         (mapf (lambda (limit i)
                 (let ((next (+ (or limit 0) step)))
                   (if (> next 0) next nil)))))
    (chee--query-set-arg mapf 4)))

(defun chee-query-set-file ()
  "Set the directory to search."
  (interactive)
  (chee--query-set-arg
   (lambda (el i)
     (expand-file-name
      (read-directory-name "Directory: " nil nil t (or el default-directory))))
   2))

(defun chee-query-unset-file ()
  "Unsets the file argument so the index is searched."
  (interactive)
  (chee--query-set-arg (lambda (el i) nil) 2))

(defun chee-query-toggle-file (arg)
  "Set the file argument. With prefix arg, unset it to search the
index."
  (interactive "P")
  (if arg
      (chee-query-unset-file)
    (chee-query-set-file)))

;;;###autoload
(defun chee-query-open (&optional arg)
  "Open the query buffer. Open query buffer in a new
window (below). With prefix argument switch to query buffer."
  (interactive "P")
  "Opens a query window."
  (let* ((query-buf (chee-query-get-buffer))
         (query-win (get-buffer-window query-buf)))
    (if (window-live-p query-win)
        (select-window query-win)
      (if arg
          (switch-to-buffer query-buf)
        (let ((win (split-window-below -9)))
          (set-window-buffer win query-buf)
          (select-window win))))))

(defun chee-query-insert-collection ()
  "Insert a collection condition of form `collection:<name>' in
the current buffer, prompting the user for a name."
  (interactive)
  (let* ((candidates (chee-proc-sync-lines (list "collection" "show" "--pattern" "~:name~%")))
         (coll (completing-read "Collection: " candidates nil t)))
    (insert "collection:'" coll "'")))

(define-key chee-query-mode-map
  (kbd "C-c C-c") chee-run-function)
(define-key chee-query-mode-map
  (kbd "C-c C-s") 'delete-window)
(define-key chee-query-mode-map
  (kbd "<tab>") 'chee-query-simple-complete-ident)
(define-key chee-query-mode-map
  (kbd "C-c i") 'chee-query-insert-collection)
(define-key chee-query-mode-map
  (kbd "C-c C-j") 'chee-query-toggle-concurrent)
(define-key chee-query-mode-map
  (kbd "C-c C-r") 'chee-query-toggle-recursive)
(define-key chee-query-mode-map
  (kbd "C-c C-l") 'chee-query-increment-limit)
(define-key chee-query-mode-map
  (kbd "C-c C-f") 'chee-query-toggle-file)


(provide 'chee-query)
;;; chee-query.el ends here
