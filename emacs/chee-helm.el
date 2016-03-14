;;; chee-helm.el --- helm sources

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
(require 'helm-mode)
(require 'helm-source)
(require 'chee-query)

(defun  chee-query-helm-complete-ident ()
  "Perform ident completion using helm interface."
  (interactive)
  (chee--query-complete-ident
   (lambda (meat)
     (let ((str (helm-comp-read "Ident: " chee-query-identifiers
                                :must-match t
                                :nomark t
                                :initial-input (if (s-blank? meat) "" (concat "^" meat)))))
       (delete-region (- (point) (length meat)) (point))
       (insert str)))
   chee-query-identifiers))

(defun chee-query-helm-set-file ()
  "Set the directory to search."
  (interactive)
  (chee--query-set-arg
   (lambda (el i)
     (expand-file-name
      (helm-read-file-name "Directory: "
                           :initial-input (or el default-directory)
                           :must-match t
                           :nomark t
                           :test 'file-directory-p)))
   2))

(defvar chee-query-helm-source-collection
  (helm-build-sync-source "Find a collection"
    :candidates
    (lambda ()
      (chee-proc-sync-sexp
       '("collection" "show" "--pattern" "(:name ~\"~:name :title ~\"~:title :description ~\"~:description)")))
    :candidate-transformer
    (lambda (candidates)
      (-map (lambda (cand)
              (cons (format "%s - %s"
                            (plist-get cand :name)
                            (plist-get cand :title))
                    cand))
            candidates))
    :delayed t
    :persistent-action
    (lambda (cand)
      (with-current-buffer (get-buffer-create " *chee-collection-help*")
        (pop-to-buffer (current-buffer))
        (erase-buffer)
        (insert (plist-get cand :description) "\n")))
    :action '(("Insert condition" .
               (lambda (cand)
                 (insert "collection:'" (plist-get cand :name) "' ")))))
  "Helm source for searching collections.")

(defun chee-query-helm-insert-collection-condition ()
  "Insert a collection to the current query prompting the user."
  (interactive)
  (helm :sources 'chee-query-helm-source-collection
        :buffer "*helm collection*"))


(define-key chee-query-mode-map (kbd "<tab>") 'chee-query-helm-complete-ident)
(define-key chee-query-mode-map (kbd "C-c i") 'chee-query-helm-insert-collection-condition)


(provide 'chee-helm)
;;; chee-helm.el ends here
