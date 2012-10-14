;;; shampoo-dict.el --- Shampoo aux dictionary object
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(defun make-shampoo-dict ()
  (make-hash-table))

(defun shampoo-dict-put (key value data)
  (puthash key value data))

(defun shampoo-dict-apply (key f data)
  (let ((v (shampoo-dict-get key data)))
    (shampoo-dict-put key (funcall f v) data)))

(defun shampoo-dict-apply-many (keys f data)
  (dolist (k keys)
    (shampoo-dict-apply k f data))
  data)

(defun shampoo-dict-get (key data)
  (gethash key data))

(defun shampoo-dict-binder-for-regexp (class-data)
  (lexical-let ((d class-data))
    (lambda (key value)
      (shampoo-dict-put key value d))))

(provide 'shampoo-dict)

;;; shampoo-dict.el end here.
