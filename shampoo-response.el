;;; shampoo-response.el --- Shampoo response object
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'cl)
(require 'shampoo-xml)

;; Currently response data is represented as a result of
;; xml-parse-region.

(defstruct shampoo-response attrs data)

(defun shampoo-response-from (str)
  (let ((xml (shampoo-parse-xml str)))
    (make-shampoo-response
     :attrs (shampoo-xml-attrs-hash (cadr xml))
     :data  (cddr xml))))

(defun shampoo-response-type (resp)
  (shampoo-response-attr 'type resp))

(defun shampoo-response-id (resp)
  (string-to-number (shampoo-response-attr 'id resp)))

(defun shampoo-response-attr (name resp)
  (gethash name (shampoo-response-attrs resp)))

(defun shampoo-response-items (resp)
  (shampoo-response-data resp))

(defun shampoo-response-items-named (name resp)
  (shampoo-xml-nodes-named name (shampoo-response-data resp)))

(defun shampoo-response-enclosed-string (resp)
  (first (shampoo-response-data resp)))

(defun shampoo-response-aggr-item (data-item)
  (when (listp data-item)
    (caddr data-item)))

(provide 'shampoo-response)

;;; shampoo-response.el ends here.
