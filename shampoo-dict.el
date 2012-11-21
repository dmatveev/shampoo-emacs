;;; shampoo-dict.el --- Shampoo aux dictionary object
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))

(defun make-shampoo-dict ()
  (make-hash-table))

(defun* shampoo-dict-put (&key key value into)
  (puthash key value into))

(defun shampoo-dict-drop (key data)
  (remhash key data))

(defun shampoo-dict-apply (key f data)
  (let ((v (shampoo-dict-get key data)))
    (shampoo-dict-put :key key :value (funcall f v) :into data)))

(defun shampoo-dict-apply-many (keys f data)
  (dolist (k keys)
    (shampoo-dict-apply k f data))
  data)

(defun shampoo-dict-get (key data)
  (gethash key data))

(defun shampoo-dict-has (key data)
  (not (null (shampoo-dict-get key data))))

(defun shampoo-dict-binder-for-regexp (class-data)
  (lexical-let ((d class-data))
    (lambda (key value)
      (shampoo-dict-put :key key :value value :into d))))

(provide 'shampoo-dict)

;;; shampoo-dict.el ends here.
