(require 'cl)
(require 'xml)

(provide 'shampoo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *shampoo* nil)
(defvar *shampoo-current-namespace* nil)
(defvar *shampoo-current-class* nil)

(defconst *shampoo-buffer-info*
  '(("Namespaces" "*shampoo-namespaces*" shampoo-open-namespace-from-buffer)
    ("Classes"    "*shampoo-classes*"    shampoo-open-class-from-buffer)
    ("Categories" "*shampoo-categories*" shampoo-open-cat-from-buffer)
    ("Methods"    "*shampoo-methods*"    shampoo-open-method-from-buffer)))

(defconst *shampoo-class-template*
  '(("instanceVariableNames:" instvar)
    ("classVariableNames:"    classvar)
    ("poolDictionaries:"      poolvar)))

(defconst *shampoo-response-handlers*
  '(("MethodSource"        shampoo-process-source-response)
    ("OperationalResponse" shampoo-process-operational-response)
    ("Class"               shampoo-process-class-response)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shampoo-this-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun shampoo-xml (tagname attrs &optional text)
  (with-output-to-string
    (princ (concat "<" (symbol-name tagname)))
    (mapcar (lambda (attr)
              (if (keywordp attr)
                  (princ (concat " " (substring (symbol-name attr) 1) "=\""))
                (progn (princ attr)
                       (princ "\""))))
            attrs)
    (if text
        (princ (concat ">" text "</" (symbol-name tagname) ">"))
      (princ " />"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode shampoo-namespaces-list-mode
  text-mode "Shampoo namespaces")

(defun shampoo-open-namespace-from-buffer ()
  (interactive)
  (setq *shampoo-current-namespace* (shampoo-this-line))
  (process-send-string
   *shampoo*
   (shampoo-xml 'request
                `(:id 1 :type "Classes"
                  :namespace ,*shampoo-current-namespace*))))

(define-key shampoo-namespaces-list-mode-map
  [return] 'shampoo-open-namespace-from-buffer)


(define-derived-mode shampoo-classes-list-mode
  text-mode "Shampoo classes")

(defun shampoo-open-class-from-buffer ()
  (interactive)
  (setq *shampoo-current-class* (shampoo-this-line))
  (process-send-string
   *shampoo*
   (shampoo-xml 'request
                `(:id 1 :type "Class"
                  :namespace ,*shampoo-current-namespace*
                  :class ,*shampoo-current-class*
                  :side "instance")))
  (process-send-string
   *shampoo*
   (shampoo-xml 'request
                `(:id 1 :type "Categories"
                  :namespace ,*shampoo-current-namespace*
                  :class ,*shampoo-current-class*
                  :side "instance"))))

(define-key shampoo-classes-list-mode-map
  [return] 'shampoo-open-class-from-buffer)


(define-derived-mode shampoo-cats-list-mode
  text-mode "Shampoo categories")

(defun shampoo-open-cat-from-buffer ()
  (interactive)
  (process-send-string
   *shampoo*
   (shampoo-xml 'request
                `(:id 1 :type "Methods"
                  :namespace ,*shampoo-current-namespace*
                  :class ,*shampoo-current-class*
                  :category ,(shampoo-this-line)
                  :side "instance"))))

(define-key shampoo-cats-list-mode-map
  [return] 'shampoo-open-cat-from-buffer)


(define-derived-mode shampoo-methods-list-mode
  text-mode "Shampoo methods")

(defun shampoo-open-method-from-buffer ()
  (interactive)
  (process-send-string
   *shampoo*
   (shampoo-xml 'request
                `(:id 1 :type "MethodSource"
                  :namespace ,*shampoo-current-namespace*
                  :class ,*shampoo-current-class*
                  :method ,(shampoo-this-line)
                  :side "instance"))))

(define-key shampoo-methods-list-mode-map
  [return] 'shampoo-open-method-from-buffer)


(define-derived-mode shampoo-code-mode
  text-mode "Shampoo code")


(defun shampoo-compile-code ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer "*shampoo-code*"))
    (process-send-string
     *shampoo*
     (shampoo-xml 'request
                  `(:id 1 :type "CompileMethod"
                    :namespace ,*shampoo-current-namespace*
                    :class ,*shampoo-current-class*
                    :side "instance")
                  (buffer-substring (point-min) (point-max))))))

(define-key shampoo-code-mode-map
  "\C-c\C-c" 'shampoo-compile-code)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro shampoo-generate-layout-impl (surface &rest data-list)
  (let ((decls '()) (commands '()) (buffa (make-symbol "buffa")))
    (dolist (data data-list)
      (destructuring-bind (sym wnd bfr code mode) data
        (pushnew `(let ((,buffa (get-buffer-create ,bfr)))
                    (set-window-buffer ,wnd ,buffa)
                    (save-excursion
                      (set-buffer ,buffa)
                      (,mode))
                    (puthash ',sym ,wnd surface))
                 commands)
        (pushnew `(,wnd ,code) decls)))
    `(let* (,@decls) ,@commands)))

(defun shampoo-create-layout ()
  (delete-other-windows)
  (let ((root (selected-window))
        (surface (make-hash-table)))
    (shampoo-generate-layout-impl
     surface
     (nspc-w nspc "*shampoo-namespaces*" root shampoo-namespaces-list-mode)
     (mets-w meth "*shampoo-methods*"    (split-window cats nil t) shampoo-methods-list-mode)
     (clss-w clss "*shampoo-classes*"    (split-window root nil t) shampoo-classes-list-mode)
     (cats-w cats "*shampoo-categories*" (split-window root nil t) shampoo-cats-list-mode)
     (code-w code "*shampoo-code*"       (split-window root) shampoo-code-mode))
    surface))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Networking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shampoo-prepare-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-working-buffer*"))
    (delete-region (point-min) (point-max))))

(defun shampoo-connect (server port)
  (interactive "sServer: \nnPort: ")
  (message "Shampoo: connecting to %s:%d..." server port)
  (let ((process (open-network-stream "shampoo" nil server port)))
    (shampoo-create-layout)
    (shampoo-prepare-buffer)
    (set-process-filter process 'shampoo-response-processor)
    (setq *shampoo* process)
    (process-send-string *shampoo* (shampoo-xml 'request '(:id 1 :type "Namespaces")))
    process))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; XML processing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shampoo-is-complete-response ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "</response>" nil t)))

(defun shampoo-response-processor (proc string)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-working-buffer*"))
    (insert string)
    (let ((this-response-end (shampoo-is-complete-response)))
      (while this-response-end
        (let ((response (xml-parse-region (point-min) this-response-end)))
          (delete-region (point-min) this-response-end)
          (shampoo-process-response (car response))
          (setq this-response-end (shampoo-is-complete-response)))))))

(defun shampoo-xml-attrs-hash (xml-attrs-list)
  (let ((result (make-hash-table :test 'equal)))
    (dolist (pair xml-attrs-list)
      (puthash (car pair) (cdr pair) result))
    result))

(defun shampoo-xml-nodes-named (symbol data)
  (remove-if (lambda (x)
               (or (stringp x)
                   (not (equal (car x) symbol))))
             data))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Response processing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shampoo-process-aggregate-response (attrs fields buffer-info)
  (destructuring-bind (buffer-name buffer-selector) buffer-info
    (save-excursion
      (set-buffer (get-buffer-create buffer-name))
      (delete-region (point-min) (point-max))
      (dolist (item fields)
        (when (listp item)
          (insert (caddr item))
          (newline)))
      (goto-line 1)
      (funcall buffer-selector))))

(defun shampoo-process-source-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (delete-region (point-min) (point-max))
    (insert (car data))))

(defun shampoo-process-class-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (delete-region (point-min) (point-max))
    (insert (concat (gethash 'superclass attrs) " subclass: #" (gethash 'class attrs)))
    (newline)
    (dolist (each *shampoo-class-template*)
      (let* ((nodes (shampoo-xml-nodes-named (cadr each) data))
             (join (lambda (a b) (concat a " " b)))
             (text (if nodes (reduce join (mapcar 'caddr nodes)) "")))
        (insert (concat "    " (car each) " '" text "'"))
        (newline)))))

(defun shampoo-process-operational-response (attrs data)
  (let ((status (gethash 'status attrs)))
    (message
     (concat "Shampoo: operation "
             (cond ((equal status "success") "successful")
                   ((equal status "failure") "failed"))))))

(defun shampoo-process-response (response)
  (let* ((attrs (shampoo-xml-attrs-hash (cadr response)))
         (type (gethash 'type attrs))
         (data (cddr response))
         (buffer (cdr (assoc type *shampoo-buffer-info*)))
         (handler (assoc type *shampoo-response-handlers*)))
    (if handler (funcall (cadr handler) attrs data)
      (shampoo-process-aggregate-response attrs data buffer))))
