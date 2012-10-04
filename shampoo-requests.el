;;; shampoo-requests.el --- Shampoo request builders
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'cl)
(require 'shampoo-dict)
(require 'shampoo-xml)

(defun* shampoo-make-login-rq (&key id user encd-pass)
  (shampoo-xml
   'request
   `(:id ,id :type "Login") nil
   (list (shampoo-xml
          'creds `(:login ,user :magic ,encd-pass)))))

(defun* shampoo-make-namespaces-rq (&key id)
  (shampoo-xml
   'request
   `(:id ,id :type "Namespaces")))

(defun* shampoo-make-classes-rq (&key id ns)
  (shampoo-xml
   'request
   `(:id ,id :type "Classes" :namespace ,ns)))

(defun* shampoo-make-cats-rq (&key id ns class side)
  (shampoo-xml
   'request
   `(:id ,id :type "Categories"
     :namespace ,ns :class ,class :side ,side)))

(defun* shampoo-make-class-rq (&key id ns class side)
  (shampoo-xml
   'request
   `(:id ,id :type "Class"
     :namespace ,ns :class ,class :side ,side)))

(defun* shampoo-make-methods-rq (&key id ns class category side)
  (shampoo-xml
   'request
   `(:id ,id :type "Methods"
     :namespace ,ns :class ,class :category ,category :side ,side)))

(defun* shampoo-make-method-rq (&key id ns class method side)
  (shampoo-xml
   'request
   `(:id ,id :type "MethodSource"
     :namespace ,ns :class ,class :side ,side
     :method ,(shampoo-escape-xml method))))

(defun* shampoo-make-compile-instance-rq (&key id ss side ns desc)
  (let* ((inst (shampoo-dict-get :instvars  desc))
         (clss (shampoo-dict-get :classvars desc))
         (pool (shampoo-dict-get :poolvars  desc))
         (fields
          (loop for data in (list inst clss pool)
                for type in '(instvar classvar poolvar)
                collect (shampoo-wrap-rq-items type data))))
    (shampoo-xml
     'request
     `(:id ,id :type "CompileClass" :superspace ,ss
       :side ,side :namespace ,ns
       :super ,(shampoo-dict-get :super desc)
       :class ,(shampoo-dict-get :name desc))
     nil
     (apply (shampoo-curry 'concatenate 'list) fields))))

(defun* shampoo-make-compile-class-rq (&key id ss side ns desc)
  (let* ((inst (shampoo-dict-get :instvars desc)))
    (shampoo-xml
     'request
     `(:id ,id :type "CompileClass" :superspace ,ss
       :side ,side :namespace ,ns
       :class ,(shampoo-dict-get :name desc))
     nil
     (shampoo-wrap-rq-items 'instvar inst))))

(defun* shampoo-make-compile-method-rq (&key id ns class side code)
  (shampoo-xml
   'request
   `(:id 1 :type "CompileMethod"
     :namespace ,ns :class ,class :side ,side)
   code))

(defun* shampoo-make-eval-rq (&key id type code)
  (shampoo-xml
   'request
   `(:id ,id :type ,type)
   code))

(defun shampoo-make-rq-item (type value)
  (shampoo-xml type nil value))

(defun shampoo-wrap-rq-items (type values)
  (mapcar (shampoo-curry 'shampoo-make-rq-item type) values))

(provide 'shampoo-requests)

;;; shampoo-requests.el ends here.
