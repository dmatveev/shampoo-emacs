(require 'cl)
(require 'xml)

(provide 'shampoo)



;; Globals -----------------------------------------------------------

(defvar *shampoo* nil)
(defvar *shampoo-current-namespace* nil)
(defvar *shampoo-current-class* nil)



;; Utils -------------------------------------------------------------

(defun shampoo-this-line ()
  (interactive)
  (buffer-substring (line-beginning-position) (line-end-position)))

(defmacro shampoo-xml (tagname attrs &rest subnodes)
  `(with-output-to-string
     (princ "<")
     (princ ,tagname)
     ,@(mapcar (lambda (attr)
                 (if (keywordp attr)
                     `(princ ,(concat " " (substring (symbol-name attr) 1) "=\""))
                   `(progn (princ ,attr)
                           (princ "\""))))
               attrs)
     (princ " />")))



;; Modes -------------------------------------------------------------

(define-derived-mode shampoo-namespaces-list-mode
  text-mode "Shampoo namespaces")

(defun shampoo-open-namespace-from-buffer ()
  (interactive)
  (setq *shampoo-current-namespace* (shampoo-this-line))
  (process-send-string
   *shampoo*
   (shampoo-xml 'request
                (:id 1 :type "Classes"
                 :namespace *shampoo-current-namespace*))))

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
                (:id 1 :type "Categories"
                 :namespace *shampoo-current-namespace*
                 :class *shampoo-current-class*
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
                (:id 1 :type "Methods"
                 :namespace *shampoo-current-namespace*
                 :class *shampoo-current-class*
                 :category (shampoo-this-line)
                 :side "instance"))))

(define-key shampoo-cats-list-mode-map
  [return] 'shampoo-open-cat-from-buffer)


(define-derived-mode shampoo-methods-list-mode
  text-mode "Shampoo methods")

(defun shampoo-open-method-from-buffer ()
  (interactive)
  (process-send-string
   *shampoo*
   (shampoo-xml request
                (:id 1 :type "MethodSource"
                 :namespace *shampoo-current-namespace*
                 :class *shampoo-current-class*
                 :method (shampoo-this-line)
                 :side "instance"))))

(define-key shampoo-methods-list-mode-map
  [return] 'shampoo-open-method-from-buffer)



;; Layout ------------------------------------------------------------

(defmacro shampoo-generate-layout-impl (surface &rest data-list)
  (let ((decls '()) (commands '()))
    (dolist (data data-list)
      (destructuring-bind (sym wnd bfr code) data
        (pushnew `(puthash ',sym ,wnd surface) commands)
        (pushnew `(set-window-buffer ,wnd (get-buffer-create ,bfr)) commands)
        (pushnew `(,wnd ,code) decls)))
    `(let* (,@decls) ,@commands)))

(defun shampoo-create-layout ()
  (interactive)
  (delete-other-windows)
  (let ((root-window (selected-window))
        (surface (make-hash-table)))
    (shampoo-generate-layout-impl
     surface
     (nspc-w nspc-window "*shampoo-namespaces*" root-window)
     (mets-w meth-window "*shampoo-methods*"    (split-window cats-window nil t))
     (clss-w clss-window "*shampoo-classes*"    (split-window root-window nil t))
     (cats-w cats-window "*shampoo-categories*" (split-window root-window nil t))
     (code-w code-window "*shampoo-code*"       (split-window root-window)))
    surface))



;; Networking --------------------------------------------------------

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
    (process-send-string *shampoo* (shampoo-xml 'request (:id 1 :type "Namespaces")))
    process))



;; XML processing ----------------------------------------------------

(defun shampoo-is-complete-response ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "</response>" nil t)))

(defun shampoo-response-processor (proc string)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-working-buffer*"))
    (insert string)
    (when (shampoo-is-complete-response)
      (let ((requests nil))
        (setq requests (xml-parse-region (point-min) (point-max)))
        (delete-region (point-min) (point-max))
        (when requests (dolist (r requests) (shampoo-process-response r)))))))

(defun shampoo-xml-attrs-hash (xml-attrs-list)
  (let ((result (make-hash-table)))
    (dolist (pair xml-attrs-list)
      (puthash (car pair) (cdr pair) result))
    result))



;; Response processing -----------------------------------------------

(defun shampoo-process-aggregate-response (attrs fields buffer-name)
  (save-excursion
    (set-buffer (get-buffer-create buffer-name))
    (delete-region (point-min) (point-max))
    (dolist (item fields)
      (when (listp item)
        (insert (caddr item))
        (newline)))))

(defun shampoo-process-source-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (delete-region (point-min) (point-max))
    (insert (car data))))

(defun shampoo-process-response (request)
  (let* ((attrs (shampoo-xml-attrs-hash (cadr request)))
         (type (gethash 'type attrs))
         (data (cddr request))
         (buffer (cdr (assoc type '(("Namespaces" . "*shampoo-namespaces*")
                                    ("Namespaces" . "*shampoo-namespaces*")
                                    ("Classes"    . "*shampoo-classes*")
                                    ("Categories" . "*shampoo-categories*")
                                    ("Methods"    . "*shampoo-methods*"))))))
    (cond ((equal type "MethodSource") (shampoo-process-source-response attrs data))
          (t (shampoo-process-aggregate-response attrs data buffer)))))
