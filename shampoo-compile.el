;;; shampoo-compile.el --- Shampoo parse utilities
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'cl)
(require 'shampoo-dict)
(require 'shampoo-state)
(require 'shampoo-regexp)
(require 'shampoo-utils)
(require 'shampoo-response)

(defconst *class-pattern*
  '(:Wd :sp "subclass:" :sp "#" :Wd
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
  (let* ((data (make-shampoo-dict))
         (binder (shampoo-dict-binder-for-regexp data)))
    (if (shampoo-regexp-parse-and-bind code pattern bindings binder)
        (shampoo-dict-apply-many to-split 'shampoo-split-string data)
      (progn (message "Shampoo: syntax error")
             nil))))
  
(defun shampoo-parse-subclassing-message (code)
  (shampoo-parse-message
   :code code
   :pattern *class-pattern*
   :bindings '(:super :name :instvars :classvars :pooldicts :category)
   :to-split '(:instvars :classvars :pooldicts)))

(defun shampoo-compile-class-instance (class-data)
  (when class-data
    (shampoo-send-message
     (shampoo-make-compile-instance-rq
      :id (shampoo-give-id)
      :ss "Smalltalk"
      :side (shampoo-side)
      :ns (shampoo-get-current-namespace)
      :desc class-data))
    (shampoo-reload-class-list (shampoo-dict-get :name class-data))))
  
(defun shampoo-parse-class-side-message (code)
  (shampoo-parse-message
   :code code
   :pattern *class-side-pattern*
   :bindings '(:name :instvars)
   :to-split '(:instvars)))
                                 
(defun shampoo-compile-class-class (class-data)
  (when class-data
    (shampoo-send-message
     (shampoo-make-compile-class-rq
      :id (shampoo-give-id)
      :ss "Smalltalk"
      :side (shampoo-side)
      :ns (shampoo-get-current-namespace)
      :desc class-data))
    (shampoo-reload-class-list (shampoo-dict-get :name class-data))))

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
  (shampoo-send-message
   (shampoo-make-compile-method-rq
    :id (shampoo-give-id)
    :ns (shampoo-get-current-namespace)
    :class (shampoo-get-current-class)
    :side (shampoo-side)
    :code (shampoo-buffer-contents "*shampoo-code*")))
  (shampoo-reload-categories-list))

(defun shampoo-print-class-message-from-response (template resp)
  (dolist (each template)
    (let* ((items (shampoo-response-items-named (cdr each) resp))
           (strs (mapcar 'shampoo-response-aggr-item items))
           (text (if strs (shampoo-join-strings " " strs) "")))
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
  ;; TODO (shampoo-get-current-namespace) may differ from the
  ;; actual class namespace, i.e. when user has switched to
  ;; other namespace before the response arrived. This thing
  ;; should be fixed.
  (insert
   (format "    category: '%s'" (shampoo-get-current-namespace))))

(defun shampoo-print-class-class-from-response (resp)
  (insert (format "%s class" (shampoo-response-attr 'class resp)))
  (newline)
  (shampoo-print-class-message-from-response
   *shampoo-class-side-template*
   resp))

(defun shampoo-handle-class-response (resp)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (erase-buffer)
    (setq header-line-format (shampoo-make-header))
    (if (shampoo-side-is :instance)
        (shampoo-print-class-instance-from-response resp)
      (shampoo-print-class-class-from-response resp))))

(provide 'shampoo-compile)

;;; shampoo-compile.el ends here.
