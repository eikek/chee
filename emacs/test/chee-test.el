(require 'chee)
(require 'dash)
(require 'ert)

;; font-lock for ert-deftest

(defun chee-test-fontlock ()
  (eval-after-load "lisp-mode"
    '(progn
       (let ((keywords '("ert-deftest" "new-query-buffer")))
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "\\_<" (regexp-opt keywords 'paren) "\\_>")
                                                     1 font-lock-keyword-face)) 'append))
       (--each (buffer-list)
         (with-current-buffer it
           (when (and (eq major-mode 'emacs-lisp-mode)
                      (boundp 'font-lock-mode)
                      (font-lock-mode))
             (font-lock-refresh-defaults)))))))

(chee-test-fontlock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defmacro new-query-buffer (&rest body)
  `(progn
     (kill-buffer (chee-query-get-buffer))
     (unwind-protect
         (progn ,@body)
       (kill-buffer (chee-query-get-buffer)))))

(put 'new-query-buffer 'lisp-indent-function 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(ert-deftest chee-map-index-test ()
  (let* ((ns (list 1 2 3 4 5))
         (mapped (chee-map-index ns '* 2 4)))
    (should (equal mapped '(1 2 6 4 20)))))


(ert-deftest chee-query-set-args-test ()
  (let ((expected (concat "# File: <index> (C-c C-f)\n"
                          "# Repository: <global> (C-c C-n)\n"
                          "# [X] --concurrent (C-c C-j)  [ ] --recursive (C-c C-r)      [100] page (M-n, M-p)\n"
                          "# [ ] --decrypt (C-c C-d)     [ default] --method (C-c C-t)\n"
                          "ext:jpg")))
    (new-query-buffer
      (chee-query-set-args "ext:jpg" t nil nil 100 nil "default" nil)
      (should
       (equal expected
              (with-current-buffer (chee-query-get-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))))
  (let ((expected (concat "# File: /a/b (C-c C-f)\n"
                          "# Repository: <global> (C-c C-n)\n"
                          "# [ ] --concurrent (C-c C-j)  [ ] --recursive (C-c C-r)      [ 10] page (M-n, M-p)\n"
                          "# [ ] --decrypt (C-c C-d)     [ default] --method (C-c C-t)\n"
                          "ext:jpg")))
    (new-query-buffer
      (chee-query-set-args "ext:jpg" nil "/a/b" nil 10 nil "default" nil)
      (should
       (equal expected
              (with-current-buffer (chee-query-get-buffer)
                (buffer-substring-no-properties (point-min) (point-max)))))))
  (let ((expected (concat "# File: <index> (C-c C-f)\n"
                          "# Repository: <global> (C-c C-n)\n"
                          "# [X] --concurrent (C-c C-j)  [ ] --recursive (C-c C-r)      [100] page (M-n, M-p)\n"
                          "# [X] --decrypt (C-c C-d)     [ default] --method (C-c C-t)\n"
                          "ext:jpg")))
    (new-query-buffer
     (chee-query-set-args "ext:jpg" t nil nil 100 t "default" nil)
     (should
      (equal expected
             (with-current-buffer (chee-query-get-buffer)
               (buffer-substring-no-properties (point-min) (point-max)))))))
  (let ((expected (concat "# File: <index> (C-c C-f)\n"
                          "# Repository: <global> (C-c C-n)\n"
                          "# [X] --concurrent (C-c C-j)  [ ] --recursive (C-c C-r)      [100] page (M-n, M-p)\n"
                          "# [X] --decrypt (C-c C-d)     [password] --method (C-c C-t)\n"
                          "ext:jpg")))
    (new-query-buffer
     (chee-query-set-args "ext:jpg" t nil nil 100 t "password" nil)
     (should
      (equal expected
             (with-current-buffer (chee-query-get-buffer)
               (buffer-substring-no-properties (point-min) (point-max)))))))
  (let ((expected (concat "# File: <index> (C-c C-f)\n"
                          "# Repository: /a/b/c (C-c C-n)\n"
                          "# [X] --concurrent (C-c C-j)  [ ] --recursive (C-c C-r)      [100] page (M-n, M-p)\n"
                          "# [ ] --decrypt (C-c C-d)     [password] --method (C-c C-t)\n"
                          "ext:jpg")))
    (new-query-buffer
     (chee-query-set-args "ext:jpg" t nil nil 100 nil "password" "/a/b/c")
     (should
      (equal expected
             (with-current-buffer (chee-query-get-buffer)
               (buffer-substring-no-properties (point-min) (point-max))))))))


(ert-deftest chee-query-set-args-restore-point-test ()
  (new-query-buffer
    (chee-query-set-args "ext:jpg" nil "/a/b" nil 10 nil "default" nil)
    (let ((pos (with-current-buffer (chee-query-get-buffer)
                 (backward-char 2)
                 (point))))
      (chee-query-set-args "ext:jpg" nil "/a/b" nil 40 nil "default" nil)
      (should
       (equal pos (with-current-buffer (chee-query-get-buffer) (point)))))))

(ert-deftest chee-query-get-args-test ()
  (new-query-buffer
    (let ((args '(("coll:mine ext:jpg" nil "/a/b" nil 10 nil "default" nil)
                  ("" nil nil t nil nil "default" nil)
                  ("" nil nil nil nil nil "default" nil)
                  ("ext:jpg" t nil nil 100 nil "default" nil))))
      (--each args
        (apply 'chee-query-set-args it)
        (should (equal it (chee-query-get-args)))))))
