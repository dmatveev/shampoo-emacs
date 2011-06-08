(require 'cl)
(require 'xml)

(provide 'shampoo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *shampoo* nil)
(defvar *shampoo-current-namespace* nil)
(defvar *shampoo-current-class* nil)
(defvar *shampoo-code-compile* nil)
(defvar *shampoo-current-side* :instance)
(defvar *shampoo-current-server* nil)
(defvar *shampoo-current-user* nil)
(defvar *shampoo-current-port* nil)

(defconst *shampoo-buffer-info*
  '(("Namespaces" . "*shampoo-namespaces*")
    ("Classes"    . "*shampoo-classes*"   )
    ("Categories" . "*shampoo-categories*")
    ("Methods"    . "*shampoo-methods*"   )))

(defconst *shampoo-class-template*
  '(("instanceVariableNames:" . instvar)
    ("classVariableNames:"    . classvar)
    ("poolDictionaries:"      . poolvar)))

(defconst *shampoo-class-side-template*
  '(("instanceVariableNames:" . instvar)))

(defconst *shampoo-response-handlers*
  '(("MethodSource"        . shampoo-process-source-response)
    ("OperationalResponse" . shampoo-process-operational-response)
    ("Class"               . shampoo-process-class-response)
    ("Info"                . shampoo-process-server-info-response)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shampoo-this-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun shampoo-clear-buffer (buffer-name)
  (save-excursion
    (set-buffer (get-buffer buffer-name))
    (let ((buffer-read-only nil))
      (erase-buffer))))

(defun shampoo-split-string-list (string)
  (if (null string) '()
    (remove-if (lambda (x) (equal x "")) (split-string string "\s"))))

(defun shampoo-side-sym-as-param (sym)
  (cdr (assoc sym '((:instance . "instance")
                    (:class    . "class")))))

(defun shampoo-side ()
  (shampoo-side-sym-as-param *shampoo-current-side*))


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

(defun shampoo-xml (tagname attrs &optional text subnodes)
  (with-output-to-string
    (princ (concat "<" (symbol-name tagname)))
    (mapcar (lambda (attr)
              (if (keywordp attr)
                  (princ (concat " " (substring (symbol-name attr) 1) "=\""))
                (progn (princ attr)
                       (princ "\""))))
            attrs)
    (if (or text subnodes)
        (progn
          (princ ">")
          (when text (princ (shampoo-escape-xml text)))
          (when subnodes (dolist (subnode subnodes) (princ subnode)))
          (princ (concat "</" (symbol-name tagname) ">")))
      (princ " />"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode shampoo-list-mode
  text-mode "Shampoo generic mode for list buffers"
  (setq buffer-read-only t)
  (make-local-variable 'set-current-item)
  (make-local-variable 'produce-request)
  (make-local-variable 'dependent-buffer)
  (make-local-variable 'update-source-buffer)
  (make-local-variable 'force-update-buffer)
  (make-local-variable 'code-compile)
  (setq force-update-buffer nil
        code-compile 'shampoo-compile-method))

(defun shampoo-update-current-side ()
  (save-excursion
    (set-buffer (get-buffer "*shampoo-categories*"))
    (setq header-line-format
          (concat (shampoo-side-sym-as-param *shampoo-current-side*) " side"))))

(defun shampoo-open-from-list ()
  (interactive)
  (let ((this-line (shampoo-this-line)))
    (when (not (equal this-line ""))
      (when (boundp 'set-current-item) (funcall set-current-item this-line))
      (process-send-string
       *shampoo*
       (funcall produce-request this-line)))))

(defun shampoo-toggle-side ()
  (interactive)
  (setq *shampoo-current-side*
        (cdr (assoc *shampoo-current-side* '((:instance . :class)
                                             (:class . :instance)))))
  (shampoo-update-current-side)
  (save-excursion
    (set-buffer (get-buffer "*shampoo-classes*"))
    (process-send-string *shampoo* (funcall produce-request (shampoo-this-line)))
    (process-send-string *shampoo* (funcall update-source-buffer))))

(defun shampoo-clear-buffer-with-dependent ()
  (let ((buffer-read-only nil))
    (erase-buffer)
    (when (boundp 'dependent-buffer)
      (shampoo-clear-buffer-by-name-with-dependent dependent-buffer))))

(defun shampoo-clear-buffer-by-name-with-dependent (buffer-name)
  (save-excursion
    (set-buffer (get-buffer buffer-name))
    (shampoo-clear-buffer-with-dependent)))

(defun shampoo-list-on-select ()
  (interactive)
  (setq *shampoo-code-compile* code-compile)
  (when (boundp 'dependent-buffer)
    (shampoo-open-from-list))
  (when (boundp 'update-source-buffer)
    (funcall update-source-buffer)))

(define-key shampoo-list-mode-map [return]   'shampoo-list-on-select)
(define-key shampoo-list-mode-map "\C-c\C-t" 'shampoo-toggle-side)

(define-derived-mode shampoo-namespaces-list-mode
  shampoo-list-mode "Shampoo namespaces"
  (setq set-current-item (lambda (x) (setq *shampoo-current-namespace* x))
        produce-request (lambda (x)
                          (shampoo-xml 'request `(:id 1 :type "Classes" :namespace ,x)))
        dependent-buffer "*shampoo-classes*"
        force-update-buffer t
        update-source-buffer
        (lambda ()
          (let ((attrs (make-hash-table)))
            (puthash 'superclass "Object" attrs)
            (puthash 'class "NameOfSubclass" attrs)
            (shampoo-process-class-response attrs '())))
        code-compile 'shampoo-compile-class))

(define-derived-mode shampoo-classes-list-mode
  shampoo-list-mode "Shampoo classes"
  (setq set-current-item (lambda (x) (setq *shampoo-current-class* x))
        produce-request
        (lambda (x)
          (shampoo-xml 'request
                       `(:id 1 :type "Categories"
                         :namespace ,*shampoo-current-namespace*
                         :class ,x :side ,(shampoo-side))))
        dependent-buffer "*shampoo-categories*"
        update-source-buffer
        (lambda ()
          (process-send-string
           *shampoo*
           (shampoo-xml 'request
                        `(:id 1 :type "Class"
                          :namespace ,*shampoo-current-namespace*
                          :class ,*shampoo-current-class* :side ,(shampoo-side)))))
        code-compile 'shampoo-compile-class))

(define-derived-mode shampoo-cats-list-mode
  shampoo-list-mode "Shampoo categories"
  (setq produce-request
        (lambda (x)
          (shampoo-xml 'request
                       `(:id 1 :type "Methods"
                         :namespace ,*shampoo-current-namespace*
                         :class ,*shampoo-current-class*
                         :category ,x :side ,(shampoo-side))))
        dependent-buffer "*shampoo-methods*"
        update-source-buffer
        (lambda ()
          (save-excursion
            (set-buffer (get-buffer "*shampoo-code*"))
            (erase-buffer)
            (insert "messageSelectorAndArgumentNames [
	\"comment stating purpose of message\"

	| temporary variable names |
	statements\n]")))))

(define-derived-mode shampoo-methods-list-mode
  shampoo-list-mode "Shampoo methods"
  (setq produce-request
        (lambda (x)
          (shampoo-xml 'request
                       `(:id 1 :type "MethodSource"
                         :namespace ,*shampoo-current-namespace*
                         :class ,*shampoo-current-class*
                         :method ,(shampoo-escape-xml x)
                         :side ,(shampoo-side))))
        update-source-buffer 'shampoo-open-from-list))

(defun shampoo-open-from-buffer-helper (buffer-name)
  (when buffer-name
    (save-excursion
      (set-buffer (get-buffer buffer-name))
      (lambda (a b) (funcall 'produce-request)))))


(define-derived-mode shampoo-code-mode
  text-mode "Shampoo code")

(defun shampoo-compile-code ()
  (interactive)
  (when *shampoo-code-compile*
    (funcall *shampoo-code-compile*)))

(define-key shampoo-code-mode-map "\C-c\C-c" 'shampoo-compile-code)
(define-key shampoo-code-mode-map "\C-c\C-t" 'shampoo-toggle-side)


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

(defun shampoo-connect (login-info)
  (interactive "sConnect to a Shampoo image: ")
  (with-temp-buffer
    (insert login-info)
    (goto-char (point-min))
    (if (re-search-forward (shampoo-build-regexp *login-pattern*) nil t)
        (let ((matches (make-hash-table)))
          (loop for sym in '(:login :server :port) for j from 1
                do (puthash sym (match-string j) matches))
          (let* ((server (gethash :server matches))
                 (port (string-to-number (gethash :port matches)))
                 (login (gethash :login matches))
                 (pass (read-passwd "Password: "))
                 (process (open-network-stream "shampoo" nil server port)))
            (setq *shampoo-current-server* server
                  *shampoo-current-port* port
                  *shampoo-current-user* login)
            (message "Shampoo: connected successfully")
            (shampoo-create-layout)
            (shampoo-prepare-buffer)
            (shampoo-update-current-side)
            (set-process-filter process 'shampoo-response-processor)
            (setq *shampoo* process)
            (process-send-string *shampoo*
                                 (shampoo-xml 'request '(:id 1 :type "Login") nil
                                              (list (shampoo-xml 'creds `(:login ,login :pass ,(md5 pass))))))
            (process-send-string *shampoo*
                                 (shampoo-xml 'request '(:id 1 :type "Namespaces")))
            process)))))

(defun shampoo-disconnect ()
  (interactive)
  (message "Shampoo: disconnected")
  (delete-process *shampoo*)
  (dolist (buffer-info *shampoo-buffer-info*)
    (shampoo-clear-buffer (cdr buffer-info)))
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
      (when (boundp 'dependent-buffer)
        (shampoo-open-from-list))
      (when (and (boundp 'update-source-buffer) force-update-buffer)
        (funcall update-source-buffer)))))

(defun shampoo-process-server-info-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (setq header-line-format
          (concat *shampoo-current-user*
                  "@" *shampoo-current-server*
                  ":" (number-to-string *shampoo-current-port*)
                  ", " (car data)))))

(defun shampoo-process-source-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (erase-buffer)
    (insert (car data))))

(defun shampoo-process-class-response (attrs data)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (erase-buffer)
    (insert
     (if (eq *shampoo-current-side* :instance)
         (concat (gethash 'superclass attrs) " subclass: #" (gethash 'class attrs))
       (concat (gethash 'class attrs) " class")))
    (newline)
    (let ((template (if (eq *shampoo-current-side* :instance)
                        *shampoo-class-template*
                      *shampoo-class-side-template*)))
      (dolist (each template)
        (let* ((nodes (shampoo-xml-nodes-named (cdr each) data))
               (join (lambda (a b) (concat a " " b)))
               (text (if nodes (reduce join (mapcar 'caddr nodes)) "")))
          (insert (concat "    " (car each) " '" text "'"))
          (newline))))
    (when (eq *shampoo-current-side* :instance)
      (insert (concat "    category: '" *shampoo-current-namespace* "'"))
      (newline))))

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
         (handler (cdr (assoc type *shampoo-response-handlers*))))
    (if handler (funcall handler attrs data)
      (shampoo-process-aggregate-response attrs data buffer))))

(defconst *class-pattern*
  '(:Wd :sp "subclass:" :sp "#" :Wd
        :sp "instanceVariableNames:" :sp "'" :Ws "'"
        :sp "classVariableNames:"    :sp "'" :Ws "'"
        :sp "poolDictionaries:"      :sp "'" :Ws "'"
        :sp "category:"              :sp "'" :Wd "'"))

(defconst *login-pattern*
  '(:Wd "@" :Wd ":" :D))

(defconst *class-side-pattern*
  '(:Wd :sp "class" :sp "instanceVariableNames:" :sp "'" :Ws "'"))

(defun shampoo-build-regexp (pattern)
  (let* ((tokens '((:Wd "\\([A-z]+\\)")
                   (:Ws "\\([A-z 0-9]*\\)")
                   (:D  "\\([0-9]*\\)")
                   (:sp "[\s\t\n]*"))))
    (reduce 'concat
            (loop for each in pattern collect
                  (let ((re (assoc each tokens)))
                    (if re (cadr re) each))))))

(defstruct shampoo-class-data
  name super instvars classvars pooldicts cat)

(defun shampoo-parse-subclassing-message ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer "*shampoo-code*"))
    (goto-char (point-min))
    (if (re-search-forward (shampoo-build-regexp *class-pattern*) nil t)
        (let ((matches (make-hash-table)))
          (loop for sym in '(:super :name :instvars :classvars :pooldicts :cat)
                for j from 1
                do (puthash sym (match-string j) matches))
          (make-shampoo-class-data
           :super     (gethash :super matches)
           :name      (gethash :name matches)
           :instvars  (shampoo-split-string-list (gethash :instvars matches))
           :classvars (shampoo-split-string-list (gethash :classvars matches))
           :pooldicts (shampoo-split-string-list (gethash :pooldicts matches))
           :cat       (gethash :cat matches)))
      (progn (message "Shampoo: syntax error")
             nil))))

(defstruct shampoo-class-side-data
  name instvars)

(defun shampoo-parse-class-side-message ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer "*shampoo-code*"))
    (goto-char (point-min))
    (if (re-search-forward (shampoo-build-regexp *class-side-pattern*) nil t)
        (let ((matches (make-hash-table)))
          (loop for sym in '(:name :instvars)
                for j from 1
                do (puthash sym (match-string j) matches))
          (make-shampoo-class-side-data
           :name      (gethash :name matches)
           :instvars  (shampoo-split-string-list (gethash :instvars matches))))
      (progn (message "Shampoo: syntax error")
             nil))))

(defun shampoo-compile-class ()
  (flet ((prod (sym) (lexical-let ((s sym)) (lambda (x) (shampoo-xml s nil x)))))
    (if (eq *shampoo-current-side* :instance)
        (let ((class-data (shampoo-parse-subclassing-message)))
          (when class-data
            (let* ((inst (mapcar (prod 'instvar)  (shampoo-class-data-instvars  class-data)))
                   (clss (mapcar (prod 'classvar) (shampoo-class-data-classvars class-data)))
                   (pool (mapcar (prod 'poolvar)  (shampoo-class-data-pooldicts class-data)))
                   (flds (concatenate 'list inst clss pool)))
              (process-send-string
               *shampoo*
               (shampoo-xml 'request
                            `(:id 1 :type "CompileClass" :superspace "Smalltalk"
                              :side ,(shampoo-side)
                              :namespace ,*shampoo-current-namespace*
                              :super ,(shampoo-class-data-super class-data)
                              :class ,(shampoo-class-data-name class-data))
                            nil flds)))))
      (let ((class-side-data (shampoo-parse-class-side-message)))
        (when class-side-data
          (let* ((inst (mapcar (prod 'instvar) (shampoo-class-side-data-instvars class-side-data))))
            (process-send-string
             *shampoo*
             (shampoo-xml 'request
                          `(:id 1 :type "CompileClass" :superspace "Smalltalk"
                            :side ,(shampoo-side)
                            :namespace ,*shampoo-current-namespace*
                            :class ,(shampoo-class-side-data-name class-side-data))
                          nil inst))))))))

(defun shampoo-compile-method ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer "*shampoo-code*"))
    (process-send-string
     *shampoo*
     (shampoo-xml 'request
                  `(:id 1 :type "CompileMethod"
                    :namespace ,*shampoo-current-namespace*
                    :class ,*shampoo-current-class*
                    :side ,(shampoo-side))
                  (buffer-substring (point-min) (point-max))))))
