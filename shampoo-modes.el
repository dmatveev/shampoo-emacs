;;; shampoo-modes.el --- Shampoo Emacs major modes
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-dict)
(require 'shampoo-state)
(require 'shampoo-state-format)
(require 'shampoo-faces)
(require 'shampoo-utils)
(require 'shampoo-compile)
(require 'shampoo-fileout)
(require 'shampoo-dialect)
(require 'shampoo-list-mode)

(define-derived-mode shampoo-working-mode
  text-mode "Shampoo mode for the working buffer"
  (shampoo-mklocal buflocal-fsm))

(defun shampoo-update-current-side ()
  (shampoo-update-header-at
   "*shampoo-categories*"
   (format "%s side" (shampoo-side))))
  
(defun shampoo-toggle-side ()
  (interactive)
  (with-~shampoo~
   (let ((current-side (shampoo-current-side ~shampoo~)))
     (setf (shampoo-current-side ~shampoo~)
           (if (eq current-side :instance) :class :instance))))
  (shampoo-update-current-side)
  (with-current-buffer "*shampoo-classes*"
    (shampoo-setq *shampoo-code-compile* (shampoo-getv code-compile))
    (shampoo-send-message
     (funcall (shampoo-getv produce-request) (shampoo-this-line)))
    (funcall (shampoo-getv update-source-buffer))))

(defun shampoo-namespaces-set-current-item (item)
  (with-~shampoo~
   (setf (shampoo-current-namespace ~shampoo~) item)))

(defun shampoo-namespaces-produce-request (item)
  (shampoo-make-classes-rq :id (shampoo-give-id) :ns item))

(defun shampoo-namespaces-update-source-buffer ()
  (let ((attrs (make-hash-table)))
    (puthash 'superclass "Object" attrs)
    (puthash 'class      "NameOfSubclass" attrs)
    (puthash 'category   (shampoo-get-current-namespace) attrs)
    (shampoo-handle-class-response
     (make-shampoo-response :attrs attrs :data '()))))

(define-derived-mode shampoo-namespaces-list-mode
  shampoo-list-mode "Shampoo namespaces"
  (shampoo-setq set-current-item 'shampoo-namespaces-set-current-item)
  (shampoo-setq produce-request  'shampoo-namespaces-produce-request)
  (shampoo-setq dependent-buffer "*shampoo-classes*")
  (shampoo-setq force-update-buffer t)
  (shampoo-setq update-source-buffer 'shampoo-namespaces-update-source-buffer)
  (shampoo-setq code-compile 'shampoo-compile-class)
  (shampoo-setq fileout-item 'shampoo-fileout-namespace))

(defun shampoo-classes-set-current-item (item)
  (with-~shampoo~
   (setf (shampoo-current-class ~shampoo~) item)))

(defun shampoo-classes-produce-request (item)
  (shampoo-make-cats-rq
   :id (shampoo-give-id)
   :ns (shampoo-get-current-namespace)
   :class item
   :side (shampoo-side)))

(defun shampoo-classes-update-source-buffer ()
  (shampoo-send-message
   (shampoo-make-class-rq
    :id (shampoo-give-id)
    :ns (shampoo-get-current-namespace)
    :class (shampoo-get-current-class)
    :side (shampoo-side))))

(define-derived-mode shampoo-classes-list-mode
  shampoo-list-mode "Shampoo classes"
  (shampoo-setq set-current-item 'shampoo-classes-set-current-item)
  (shampoo-setq produce-request 'shampoo-classes-produce-request)
  (shampoo-setq dependent-buffer "*shampoo-categories*")
  (shampoo-setq update-source-buffer 'shampoo-classes-update-source-buffer)
  (shampoo-setq code-compile 'shampoo-compile-class)
  (shampoo-setq remove-item 'shampoo-remove-class)
  (shampoo-setq fileout-item 'shampoo-fileout-class))

(defun shampoo-cats-set-current-item (item)
  (with-~shampoo~
   (setf (shampoo-current-category ~shampoo~) item)))

(defun shampoo-cats-produce-request (item)
  (shampoo-make-methods-rq
   :id (shampoo-give-id)
   :ns (shampoo-get-current-namespace)
   :class (shampoo-get-current-class)
   :category item
   :side (shampoo-side)))

(defun shampoo-cats-update-source-buffer ()
  (with-current-buffer "*shampoo-code*"
    (save-excursion
      (setq header-line-format (shampoo-make-header))
      (erase-buffer)
      (with-~shampoo~
       (insert
        (shampoo-dialect-message-template
         (shampoo-current-smalltalk ~shampoo~)))))))

(defun shampoo-cats-pre-insert-hook ()
  (insert "*")
  (newline))

(define-derived-mode shampoo-cats-list-mode
  shampoo-list-mode "Shampoo categories"
  (shampoo-setq set-current-item 'shampoo-cats-set-current-item)
  (shampoo-setq produce-request  'shampoo-cats-produce-request)
  (shampoo-setq dependent-buffer "*shampoo-methods*")
  (shampoo-setq update-source-buffer 'shampoo-cats-update-source-buffer)
  (shampoo-setq pre-insert-hook 'shampoo-cats-pre-insert-hook)
  (shampoo-setq remove-item 'shampoo-remove-category))

(define-key
  shampoo-cats-list-mode-map
  [header-line mouse-1]
  'shampoo-toggle-side)

(define-key
  shampoo-cats-list-mode-map
  "\C-cm"
  'shampoo-rename-category-from-list)

(defun shampoo-rename-category-from-list ()
  (interactive)
  (let ((this-category (shampoo-this-line)))
    (when (not (equal "" this-category))
      (shampoo-rename-category this-category))))

(defun shampoo-methods-set-current-item (item)
  (with-~shampoo~
   (shampoo-update-header-at
    "*shampoo-code*"
    (format "%s    %s"
            (shampoo-make-header)
            (shampoo-build-method-name
             (shampoo-current-class ~shampoo~)
             item)))))

(defun shampoo-methods-produce-request (item)
  (shampoo-make-method-rq
   :id (shampoo-give-id)
   :ns (shampoo-get-current-namespace)
   :class (shampoo-get-current-class)
   :method item
   :side (shampoo-side)))

(define-derived-mode shampoo-methods-list-mode
  shampoo-list-mode "Shampoo methods"
  (shampoo-setq set-current-item 'shampoo-methods-set-current-item)
  (shampoo-setq produce-request  'shampoo-methods-produce-request)
  (shampoo-setq update-source-buffer 'shampoo-open-from-list)
  (shampoo-setq remove-item 'shampoo-remove-method))

(define-key
  shampoo-methods-list-mode-map
  "\C-cm"
  'shampoo-change-method-category-from-list)

(defun shampoo-change-method-category-from-list ()
  (interactive)
  (let ((this-method (shampoo-this-line)))
    (when (not (equal "" this-method))
      (shampoo-change-method-category this-method))))

(defun shampoo-open-from-buffer-helper (buffer-name)
  (when buffer-name
    (with-current-buffer buffer-name
      (lambda (a b) (funcall (shampoo-getv produce-request))))))

;; This piece of code is adopted from the smalltalk-mode.el,
;; the part of the GNU Smalltalk distribution.
;; Thanks to its authors and contributors.

(defconst shampoo-smalltalk-binsel "\\([-+*/~,<>=&?]\\{1,2\\}\\|:=\\|||\\)"
  "Smalltalk binary selectors")

(defconst shampoo-smalltalk-font-lock-keywords
  (list
   '("#[A-z][A-z0-9_]*"          . font-lock-constant-face)
   '("\\<[A-z][A-z0-9_]*:"       . font-lock-function-name-face)
   (cons shampoo-smalltalk-binsel 'font-lock-function-name-face)
   '("\\$."                      . font-lock-string-face)
   '("\\<[A-Z]\\sw*\\>"          . font-lock-type-face)
   '("[0-9]+"                    . font-lock-constant-face))
  "Basic Smalltalk keywords font-locking")

(defconst shampoo-smalltalk-font-lock-keywords-1
  shampoo-smalltalk-font-lock-keywords	   
  "Level 1 Smalltalk font-locking keywords")

(defconst shampoo-smalltalk-font-lock-keywords-2
  (append shampoo-smalltalk-font-lock-keywords-1
	  (list 
	   '("\\<\\(true\\|false\\|nil\\|self\\|super\\)\\>" 
	     . font-lock-builtin-face)
	   '(":[a-z][A-z0-9_]*" . font-lock-variable-name-face)
	   '(" |"               . font-lock-type-face)
	   '("<.*>"             . font-lock-builtin-face)))
  "Level 2 Smalltalk font-locking keywords")

(defconst shampoo-smalltalk-font-lock-keywords-list
  '((shampoo-smalltalk-font-lock-keywords
     shampoo-smalltalk-font-lock-keywords-1
     shampoo-smalltalk-font-lock-keywords-2)))

(defvar shampoo-smalltalk-mode-syntax-table 
  (let ((table (make-syntax-table)))
    ;; Make sure A-z0-9 are set to "w   " for completeness
    (let ((c 0))
      (setq c ?0)
      (while (<= c ?9)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table))
      (setq c ?A)
      (while (<= c ?Z)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table))
      (setq c ?a)
      (while (<= c ?z)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table)))
    (modify-syntax-entry 10  " >  " table) ; Comment (generic)
    (modify-syntax-entry ?:  ".   " table) ; Symbol-char
    (modify-syntax-entry ?_  "_   " table) ; Symbol-char
    (modify-syntax-entry ?\" "!1  " table) ; Comment (generic)
    (modify-syntax-entry ?'  "\"  " table) ; String
    (modify-syntax-entry ?#  "'   " table) ; Symbol or Array constant
    (modify-syntax-entry ?\( "()  " table) ; Grouping
    (modify-syntax-entry ?\) ")(  " table) ; Grouping
    (modify-syntax-entry ?\[ "(]  " table) ; Block-open
    (modify-syntax-entry ?\] ")[  " table) ; Block-close
    (modify-syntax-entry ?{  "(}  " table) ; Array-open
    (modify-syntax-entry ?}  "){  " table) ; Array-close
    (modify-syntax-entry ?$  "/   " table) ; Character literal
    (modify-syntax-entry ?!  ".   " table) ; End message / Delimit defs
    (modify-syntax-entry ?\; ".   " table) ; Cascade
    (modify-syntax-entry ?|  ".   " table) ; Temporaries
    (modify-syntax-entry ?^  ".   " table) ; Return
    ;; Just to make sure these are not set to "w   "
    (modify-syntax-entry ?<  ".   " table) 
    (modify-syntax-entry ?>  ".   " table) 
    (modify-syntax-entry ?+  ".   " table) ; math
    (modify-syntax-entry ?-  ".   " table) ; math
    (modify-syntax-entry ?*  ".   " table) ; math
    (modify-syntax-entry ?/  ".2  " table) ; math
    (modify-syntax-entry ?=  ".   " table) ; bool/assign
    (modify-syntax-entry ?%  ".   " table) ; valid selector
    (modify-syntax-entry ?&  ".   " table) ; boolean
    (modify-syntax-entry ?\\ ".   " table) ; ???
    (modify-syntax-entry ?~  ".   " table) ; misc. selector
    (modify-syntax-entry ?@  ".   " table) ; Point
    (modify-syntax-entry ?,  ".   " table) ; concat
    table)
  "Syntax table used by Smalltalk mode")

(define-derived-mode shampoo-code-mode
  text-mode "Shampoo code"
  (set (make-local-variable 'font-lock-defaults)  
       shampoo-smalltalk-font-lock-keywords-list)
  (set-syntax-table shampoo-smalltalk-mode-syntax-table))

(defun shampoo-compile-code ()
  (interactive)
  (let ((fcn (shampoo-getv *shampoo-code-compile*)))
  (when fcn (funcall fcn))))

(define-key shampoo-code-mode-map
  "\C-c\C-c"
  'shampoo-compile-code)

(define-key shampoo-code-mode-map
  "\C-c\C-t"
  'shampoo-toggle-side)

(defun shampoo-jump-to (wnd)
  (lexical-let ((binding wnd))
    (lambda ()
      (interactive)
      (with-~shampoo~
       (select-window
        (shampoo-dict-get
         binding
         (shampoo-current-main-windows ~shampoo~)))))))

(defun shampoo-define-jump-keys (mode-map)
  (define-key mode-map "\C-cn" (shampoo-jump-to :namespaces))
  (define-key mode-map "\C-cc" (shampoo-jump-to :classes))
  (define-key mode-map "\C-c[" (shampoo-jump-to :categories))
  (define-key mode-map "\C-c]" (shampoo-jump-to :methods))
  (define-key mode-map "\C-c " (shampoo-jump-to :source)))

(defun shampoo-define-fileout-keys (mode-map)
  (define-key mode-map "\C-cfn" 'shampoo-fileout-current-namespace)
  (define-key mode-map "\C-cfc" 'shampoo-fileout-current-class)
  (define-key mode-map "\C-cfa" 'shampoo-fileout-current-class-category))

(shampoo-define-jump-keys shampoo-list-mode-map)
(shampoo-define-jump-keys shampoo-code-mode-map)

(shampoo-define-fileout-keys shampoo-list-mode-map)
(shampoo-define-fileout-keys shampoo-code-mode-map)

(provide 'shampoo-modes)

;;; shampoo-modes.el ends here.
