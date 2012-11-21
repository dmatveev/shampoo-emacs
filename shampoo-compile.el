;;; shampoo-compile.el --- Shampoo parse utilities
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-dict)
(require 'shampoo-state)
(require 'shampoo-state-format)
(require 'shampoo-regexp)
(require 'shampoo-utils)
(require 'shampoo-requests)
(require 'shampoo-response)
(require 'shampoo-dialect)
(require 'shampoo-list-mode)

(defconst *class-pattern*
  '(:Wa :sp "subclass:" :sp "#" :Wd
        :sp "instanceVariableNames:" :sp "'" :Ws "'"
        :sp "classVariableNames:"    :sp "'" :Ws "'"
        :sp "poolDictionaries:"      :sp "'" :Ws "'"
        :sp "category:"              :sp "'" :Wc "'"))

(defconst *class-side-pattern*
  '(:Wd :sp "class" :sp "instanceVariableNames:" :sp "'" :Ws "'"))

(defconst *shampoo-class-template*
  '(("instanceVariableNames:" . instvar)
    ("classVariableNames:"    . classvar)
    ("poolDictionaries:"      . poolvar)))

(defconst *shampoo-class-side-template*
  '(("instanceVariableNames:" . instvar)))

(defun* shampoo-parse-message (&key code pattern bindings to-split)
  (let* ((data   (make-shampoo-dict))
         (binder (shampoo-dict-binder-for-regexp data)))
    (if (shampoo-regexp-parse-and-bind code pattern bindings binder)
        (shampoo-dict-apply-many to-split 'shampoo-split-string data)
      (progn (message "Shampoo: syntax error")
             nil))))
  
(defun shampoo-parse-subclassing-message (code)
  (shampoo-parse-message
   :code     code
   :pattern  *class-pattern*
   :bindings '(:super :name :instvars :classvars :pooldicts :category)
   :to-split '(:instvars :classvars :pooldicts)))

(defun shampoo-compile-class-instance (class-data)
  (when class-data
    (multiple-value-bind (superspace superclass)
        (shampoo-infer-superspace class-data)
      (let ((request-id (shampoo-give-id)))
        (shampoo-subscribe
         request-id
         (shampoo-make-class-reloader
          (shampoo-dict-get :name class-data)))
        (shampoo-dict-put
         :key   :super
         :value superclass
         :into  class-data)
        (shampoo-send-message
         (shampoo-make-compile-instance-rq
          :id   request-id
          :ss   superspace
          :side (shampoo-side)
          :ns   (shampoo-get-current-namespace)
          :cat  (shampoo-dict-get :category class-data)
          :desc class-data))))))

(defun shampoo-infer-superspace (class-data)
  (let ((info
         (with-~shampoo~
          (shampoo-dialect-extract-parent
           (shampoo-current-smalltalk ~shampoo~)
           (shampoo-dict-get :super class-data)))))
  (multiple-value-bind (superspace superclass) info
    (values (if (null superspace)
                (shampoo-get-current-namespace)
              superspace)
            superclass))))                      
  
(defun shampoo-parse-class-side-message (code)
  (shampoo-parse-message
   :code     code
   :pattern  *class-side-pattern*
   :bindings '(:name :instvars)
   :to-split '(:instvars)))
                                 
(defun shampoo-compile-class-class (class-data)
  (when class-data
    (let ((request-id (shampoo-give-id)))
      (shampoo-subscribe
       request-id
       (shampoo-make-class-reloader
        (shampoo-dict-get :name class-data)))
      (shampoo-send-message
       (shampoo-make-compile-class-rq
        :id   request-id
        :ss   "Smalltalk"
        :side (shampoo-side)
        :ns   (shampoo-get-current-namespace)
        :desc class-data)))))

(defun shampoo-compile-class ()
  (interactive)
  (let ((code (shampoo-buffer-contents "*shampoo-code*")))
    (if (shampoo-side-is :instance)
        (shampoo-compile-class-instance
         (shampoo-parse-subclassing-message code))
      (shampoo-compile-class-class
       (shampoo-parse-class-side-message code)))))

(defun shampoo-compile-method ()
  (interactive)
  (let ((request-id  (shampoo-give-id))
        (current-cat (shampoo-get-current-category)))
    (shampoo-subscribe
     request-id
     (lexical-let ((cat current-cat))
       (lambda (resp)
         (when (shampoo-response-is-success resp)
           (shampoo-reload-categories-list :open-then cat
                                           :need-open t)))))
    (shampoo-send-message
     (shampoo-make-compile-method-rq
      :id       request-id
      :ns       (shampoo-get-current-namespace)
      :class    (shampoo-get-current-class)
      :side     (shampoo-side)
      :category current-cat
      :code     (shampoo-buffer-contents "*shampoo-code*")))))

(defun shampoo-print-class-message-from-response (template resp)
  (dolist (each template)
    (let* ((items (shampoo-response-items-named (cdr each) resp))
           (strs  (mapcar 'shampoo-response-aggr-item items))
           (text  (if strs (shampoo-join-strings " " strs) "")))
      (insert (format "    %s '%s'"  (car each) text))
      (newline))))

(defun shampoo-print-class-instance-from-response (resp)
  (insert
   (format "%s subclass: #%s"
           (shampoo-response-attr 'superclass resp)
           (shampoo-response-attr 'class resp)))
  (newline)
  (shampoo-print-class-message-from-response
   *shampoo-class-template*
   resp)
  (insert (format
           "    category: '%s'"
           (shampoo-response-attr 'category resp)))
  (newline))

(defun shampoo-print-class-class-from-response (resp)
  (insert (format "%s class" (shampoo-response-attr 'class resp)))
  (newline)
  (shampoo-print-class-message-from-response
   *shampoo-class-side-template*
   resp))

(defun shampoo-handle-class-response (resp)
  (with-current-buffer "*shampoo-code*"
    (save-excursion
      (erase-buffer)
      (setq header-line-format (shampoo-make-header))
      (if (shampoo-side-is :instance)
          (with-~shampoo~
           (shampoo-print-class-instance-from-response resp)
           (setf (shampoo-current-class-category ~shampoo~)
                 (shampoo-response-attr 'category resp)))
        (shampoo-print-class-class-from-response resp)))))

(defun shampoo-remove-class (class-name)
  (when (yes-or-no-p
         (format "Are you sure you want to remove class %s? "
                 class-name))
    (let ((request-id (shampoo-give-id)))
      (shampoo-subscribe
       request-id
       (lambda (resp)
         (when (shampoo-response-is-success resp)
           (shampoo-reload-class-list))))
      (shampoo-send-message
       (shampoo-make-remove-class-rq
        :id    request-id
        :ns    (shampoo-get-current-namespace)
        :class class-name)))))
  
(defun shampoo-remove-method (method-selector)
  (when (yes-or-no-p
         (format "Are you sure you want to remove method #%s? "
                 method-selector))
    (let ((request-id (shampoo-give-id)))
      (shampoo-subscribe
       request-id
       (lambda (resp)
         (when (shampoo-response-is-success resp)
           (shampoo-reload-categories-list :open-then nil
                                           :need-open t))))
      (shampoo-send-message
       (shampoo-make-remove-method-rq
        :id     request-id
        :ns     (shampoo-get-current-namespace)
        :class  (shampoo-get-current-class)
        :side   (shampoo-side)
        :method method-selector)))))

(defun shampoo-remove-category (category)
  (when (yes-or-no-p
         (format "Are you sure you want to remove category \"%s\"? "
                 category))
    (let ((request-id (shampoo-give-id)))
      (shampoo-subscribe
       request-id
       (lambda (resp)
         (when (shampoo-response-is-success resp)
           (shampoo-reload-categories-list :open-then nil
                                           :need-open t))))
      (shampoo-send-message
       (shampoo-make-remove-category-rq
        :id       request-id
        :ns       (shampoo-get-current-namespace)
        :class    (shampoo-get-current-class)
        :side     (shampoo-side)
        :category category)))))
  
(defun shampoo-change-method-category (method)
  (let ((request-id (shampoo-give-id))
        (target-cat (read-string "Change method's category to: ")))
    (shampoo-subscribe
     request-id
     (lexical-let ((cat target-cat))
       (lambda (resp)
         (when (shampoo-response-is-success resp)
           (shampoo-reload-categories-list :open-then cat
                                           :need-open t)))))
    (shampoo-send-message
     (shampoo-make-change-category-rq
      :id       request-id
      :ns       (shampoo-get-current-namespace)
      :class    (shampoo-get-current-class)
      :side     (shampoo-side)
      :method   method
      :category target-cat))))

(defun shampoo-rename-category (category)
  (let* ((request-id (shampoo-give-id))
         (prompt     (format "Rename category \"%s\" to: " category))
         (to-cat     (read-string prompt)))
    (shampoo-subscribe
     request-id
     (lexical-let ((cat to-cat))
       (lambda (resp)
         (when (shampoo-response-is-success resp)
           (shampoo-reload-categories-list :open-then cat
                                           :need-open t)))))
    (shampoo-send-message
     (shampoo-make-rename-category-rq
      :id       request-id
      :ns       (shampoo-get-current-namespace)
      :class    (shampoo-get-current-class)
      :side     (shampoo-side)
      :from     category
      :to       to-cat))))

(provide 'shampoo-compile)
  
;;; shampoo-compile.el ends here.
