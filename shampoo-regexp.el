;;; shampoo-regexp.el --- Shampoo regular expression builder
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'cl)

(defconst *shampoo-regexp-tokens*
  '((:Wd "\\([A-z]+[0-9]*\\)")
    (:Ws "\\([A-z 0-9]*\\)")
    (:Wa "\\([A-z\\.0-9]*\\)")
    (:Wc "\\([A-z\-0-9]*\\)")
    (:D  "\\([0-9]*\\)")
    (:sp "[\s\t\n]*")
    (:cr "\r")
    (:lf "\n")))

(defun shampoo-build-regexp (pattern)
  (reduce 'concat
          (loop for each in pattern collect
                (let ((re (assoc each *shampoo-regexp-tokens*)))
                  (if re (cadr re) each)))))

(defun shampoo-regexp-is-capture-token (sym)
  (let ((pattern (assoc sym *shampoo-regexp-tokens*)))
    (and (symbolp sym)
         pattern
         (string-match "^\\\\\(.*\\\\\)$" (cadr pattern)))))

(defun shampoo-regexp-num-capture-tokens (pattern)
  (loop for each in pattern sum
        (if (shampoo-regexp-is-capture-token each) 1 0)))

(defun shampoo-regexp-parse (str pattern)
  (let ((regexp (shampoo-build-regexp pattern))
        (re-len (shampoo-regexp-num-capture-tokens pattern)))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
        (loop for i from 1 to re-len collect
              (match-string i))))))

(defun shampoo-regexp-parse-and-bind (str pattern bindings binder)
  (let ((parsed (shampoo-regexp-parse str pattern)))
    (when parsed
      (loop for binding in bindings for j from 0 do
            (funcall
             binder
             binding
             (shampoo-regexp-extract j parsed)))
      t)))

(defun shampoo-regexp-extract (n parsed)
  (nth n parsed))

(provide 'shampoo-regexp)

;;; shampoo-regexp.el ends here.
