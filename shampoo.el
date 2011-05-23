(require 'cl)
(require 'xml)

(provide 'shampoo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *shampoo* nil)
(defvar *shampoo-current-namespace* nil)
(defvar *shampoo-current-class* nil)

(defconst *shampoo-buffer-info*
  '(("Namespaces" "*shampoo-namespaces*")
    ("Classes"    "*shampoo-classes*"   )
    ("Categories" "*shampoo-categories*")
    ("Methods"    "*shampoo-methods*"   )))

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

(defun shampoo-clear-buffer (buffer-name)
  (save-excursion
    (set-buffer (get-buffer buffer-name))
    (erase-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; XML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst shampoo-replace-in-string (str regexp newtext)
  (replace-regexp-in-string regexp newtext str t t))

;; This function has been taken from emacs-jabber. Thanks to its authors
(defun shampoo-escape-xml (str)
  (if (stringp str)
      (let ((newstr (concat str)))
        (setq newstr (shampoo-replace-in-string newstr "\f" "\n"))
        (setq newstr (shampoo-replace-in-string newstr "[\000-\010\013\014\016-\037]" " "))
        (setq newstr (shampoo-replace-in-string newstr "&" "&amp;"))
        (setq newstr (shampoo-replace-in-string newstr "<" "&lt;"))
        (setq newstr (shampoo-replace-in-string newstr ">" "&gt;"))
        (setq newstr (shampoo-replace-in-string newstr "'" "&apos;"))
        (setq newstr (shampoo-replace-in-string newstr "\"" "&quot;"))
        newstr)
    str))

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
        (princ (concat ">" (shampoo-escape-xml text) "</" (symbol-name tagname) ">"))
      (princ " />"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode shampoo-list-mode
  text-mode "Shampoo generic mode for list buffers"
  (setq buffer-read-only t)
  (make-local-variable 'set-current-item)
  (make-local-variable 'produce-response)
  (make-local-variable 'dependent-buffer))

(defun shampoo-open-from-list ()
  (interactive)
  (let ((this-line (shampoo-this-line)))
    (when (not (equal this-line ""))
      (when (boundp 'set-current-item) (funcall set-current-item this-line))
      (process-send-string
       *shampoo*
       (funcall produce-response this-line)))))

(defun shampoo-clear-buffer-with-dependent ()
  (let ((buffer-read-only nil))
    (erase-buffer)
    (when (boundp 'dependent-buffer)
      (shampoo-clear-buffer-by-name-with-dependent dependent-buffer))))

(defun shampoo-clear-buffer-by-name-with-dependent (buffer-name)
  (save-excursion
    (set-buffer (get-buffer buffer-name))
    (shampoo-clear-buffer-with-dependent)))

(define-key shampoo-list-mode-map
  [return] 'shampoo-open-from-list)


(define-derived-mode shampoo-namespaces-list-mode
  shampoo-list-mode "Shampoo namespaces"
  (setq set-current-item (lambda (x) (setq *shampoo-current-namespace* x)))
  (setq produce-response
        (lambda (x)
          (shampoo-xml 'request `(:id 1 :type "Classes" :namespace ,x))))
  (setq dependent-buffer "*shampoo-classes*"))

(define-derived-mode shampoo-classes-list-mode
  shampoo-list-mode "Shampoo classes"
  (setq set-current-item (lambda (x) (setq *shampoo-current-class* x)))
  (setq produce-response
        (lambda (x)
          (shampoo-xml 'request
                       `(:id 1 :type "Categories"
                        :namespace ,*shampoo-current-namespace*
                        :class ,x :side "instance"))))
  (setq dependent-buffer "*shampoo-categories*"))

(define-derived-mode shampoo-cats-list-mode
  shampoo-list-mode "Shampoo categories"
  (setq produce-response
        (lambda (x)
          (shampoo-xml 'request
                       `(:id 1 :type "Methods"
                         :namespace ,*shampoo-current-namespace*
                         :class ,*shampoo-current-class*
                         :category ,x :side "instance"))))
  (setq dependent-buffer "*shampoo-methods*"))

(define-derived-mode shampoo-methods-list-mode
  shampoo-list-mode "Shampoo methods"
  (setq produce-response
        (lambda (x)
          (shampoo-xml 'request
                       `(:id 1 :type "MethodSource"
                         :namespace ,*shampoo-current-namespace*
                         :class ,*shampoo-current-class*
                         :method ,(shampoo-escape-xml x)
                         :side "instance")))))

(defun shampoo-open-from-buffer-helper (buffer-name)
  (when buffer-name
    (save-excursion
      (set-buffer (get-buffer buffer-name))
      (lambda (a b) (funcall 'produce-response)))))


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
    (erase-buffer)))

(defun shampoo-connect (server port)
  (interactive "sServer: \nnPort: ")
  (message "Shampoo: connecting to %s:%d..." server port)
  (let ((process (open-network-stream "shampoo" nil server port)))
    (message "Shampoo: connected successfully")
    (shampoo-create-layout)
    (shampoo-prepare-buffer)
    (set-process-filter process 'shampoo-response-processor)
    (setq *shampoo* process)
    (process-send-string *shampoo* (shampoo-xml 'request '(:id 1 :type "Namespaces")))
    process))

(defun shampoo-disconnect ()
  (interactive)
  (message "Shampoo: disconnected")
  (delete-process *shampoo*)
  (dolist (buffer-info *shampoo-buffer-info*)
    (shampoo-clear-buffer (cadr buffer-info)))
  (shampoo-clear-buffer "*shampoo-code*"))


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

(defun shampoo-process-aggregate-response (attrs fields buffer-name)
  (save-excursion
    (set-buffer (get-buffer buffer-name))
    (let ((buffer-read-only nil))
      (shampoo-clear-buffer-with-dependent)
      (dolist (item fields)
        (when (listp item)
          (insert (caddr item))
          (newline)))
      (goto-line 1)
      (shampoo-open-from-list))))

(defun shampoo-process-source-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (erase-buffer)
    (insert (car data))))

(defun shampoo-process-class-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (erase-buffer)
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
         (buffer (cadr (assoc type *shampoo-buffer-info*)))
         (handler (cadr (assoc type *shampoo-response-handlers*))))
    (if handler (funcall handler attrs data)
      (shampoo-process-aggregate-response attrs data buffer))))
