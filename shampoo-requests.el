;;; shampoo-requests.el --- Shampoo request builders
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-dict)
(require 'shampoo-xml)
(require 'shampoo-utils)

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

(defun* shampoo-make-compile-instance-rq (&key id ss side ns cat desc)
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
       :side     ,side :namespace ,ns
       :super    ,(shampoo-dict-get :super desc)
       :class    ,(shampoo-dict-get :name desc)
       :category ,cat)
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

(defun* shampoo-make-compile-method-rq (&key id ns class side category code)
  (shampoo-xml
   'request
   `(:id ,id :type "CompileMethod"
     :namespace ,ns :class ,class :side ,side :category ,category)
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

(defun* shampoo-make-remove-class-rq (&key id ns class)
  (shampoo-xml
   'request
   `(:id ,id :type "RemoveClass"
     :namespace ,ns :class ,class)))

(defun* shampoo-make-remove-method-rq (&key id ns class side method)
  (shampoo-xml
   'request
   `(:id ,id :type "RemoveMethod"
     :namespace ,ns :class ,class :side ,side :method ,method)))

(defun* shampoo-make-remove-category-rq
    (&key id ns class side category)
  (shampoo-xml
   'request
   `(:id ,id :type "RemoveCategory"
     :namespace ,ns :class ,class :side ,side :category ,category)))

(defun* shampoo-make-change-category-rq
    (&key id ns class side category method)
  (shampoo-xml
   'request
   `(:id ,id :type "ChangeCategory"
     :namespace ,ns :class ,class :side ,side
     :category ,category :method ,method)))

(defun* shampoo-make-rename-category-rq
    (&key id ns class side from to)
  (shampoo-xml
   'request
   `(:id ,id :type "RenameCategory"
     :namespace ,ns :class ,class :side ,side
     :from ,from :to ,to)))

(defun* shampoo-make-fileout-namespace-rq (&key id ns split)
  (shampoo-xml
   'request
   `(:id ,id :type "FileOut" :namespace ,ns :splitby ,split)))

(defun* shampoo-make-fileout-class-rq (&key id ns class)
  (shampoo-xml
   'request
   `(:id ,id :type "FileOut" :namespace ,ns :class ,class)))

(defun* shampoo-make-fileout-category-rq (&key id ns cat split)
  (shampoo-xml
   'request
   `(:id ,id :type "FileOut" :namespace ,ns
     :category ,cat :splitby ,split)))

(provide 'shampoo-requests)

;;; shampoo-requests.el ends here.
