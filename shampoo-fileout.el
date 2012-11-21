;;; shampoo-modes.el --- Shampoo FileOut routines
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-state)
(require 'shampoo-requests)
(require 'shampoo-response)
(require 'shampoo-utils)
(require 'shampoo-regexp)

;; Definitions and variables

(eval-when (compile load eval)
  (defstruct shampoo-fileout-conf
    item
    splitby
    directory
    fproc
    is-loaded))

(defvar *shampoo-fileout-scenarios* '())

;; File name manipulations

(defun shampoo-filename-as-is (name)
  name)

(defun shampoo-filename-strip-package (name)
  (let ((parsed (shampoo-regexp-parse name '(:Wd "\-" :Ws))))
    (if parsed
        (shampoo-regexp-extract 1 parsed)
      name)))

(defun shampoo-filename-squash (name)
  (shampoo-join-strings 
   ""
   (mapcar 'shampoo-capitalize (shampoo-split-string name))))

(defun shampoo-fileout-transform-filename (name rebuild-functions)
  (let ((result name))
    (dolist (func rebuild-functions)
      (setq result (funcall func result)))
    result))

(defun shampoo-fileout-build-filename (name conf)
  (format
   "%s%s.st"
   (file-name-as-directory (shampoo-fileout-conf-directory conf))
   (shampoo-fileout-transform-filename
    name
    (shampoo-fileout-conf-fproc conf))))

;; Fileout backup routines

(defun shampoo-fileout-format-timestamp ()
  (destructuring-bind
      (secs mins hour day month year dow dst zone)
      (decode-time)
    (format "%04d-%02d-%02d--%02d-%02d-%02d"
            year month day hour mins secs)))

(defun shampoo-create-backup-dir (config)
  (let ((backup-dir (concat (file-name-as-directory
                             (shampoo-fileout-conf-directory config))
                            "shampoo-backup")))
    (when (not (file-exists-p backup-dir))
      (make-directory backup-dir))
    backup-dir))

(defun shampoo-fileout-backup (path conf)
  (when (file-exists-p path)
    (let* ((backup-dir  (shampoo-create-backup-dir conf))
           (base-name   (file-name-sans-extension
                         (file-name-nondirectory path)))
           (backup-name (format "%s%s-%s.st"
                                (file-name-as-directory backup-dir)
                                (shampoo-fileout-format-timestamp)
                                base-name)))
      (copy-file path backup-name 0 t))))

;; Save to disk
    
(defun* shampoo-save-fileout (&key config)
  (lexical-let ((conf config))
    (lambda (response)
      (when (not (shampoo-response-is-failure response))
        (let* ((file (shampoo-msum
                      (shampoo-response-attr 'class    response)
                      (shampoo-response-attr 'category response)))
               (path (shampoo-fileout-build-filename file conf)))
          (shampoo-fileout-backup path conf)
          (with-temp-buffer
            (insert (shampoo-response-enclosed-string response))
            (write-region nil nil path))))
      (when (shampoo-response-is-last-in-sequence response)
        (message "Shampoo: file out complete")))))

;; User interaction functions

(defun* shampoo-fileout-ask (&key what from default)
  (shampoo-ask :prompt  (format "File out %s: " what)
               :from    from
               :default default))

(defun shampoo-fileout-conf-get-splitby (fileout-subject)
  (let ((type (car fileout-subject)))
    (if (eql :class type)
        "class"
      (completing-read "Organize source code files by: "
                       '("class" "category")
                       nil t "category"))))

(defun shampoo-fileout-conf-get-directory ()
  (read-directory-name "Store files into directory: "))

(defun shampoo-fileout-conf-get-fproc (fileout-subject)
  (let ((type (car fileout-subject))
        (funcs '(("Strip package prefix from file names? "
                  . shampoo-filename-strip-package)
                 ("Remove space characters from file names? "
                  . shampoo-filename-squash))))
    (if (eql :class type)
        '(shampoo-filename-as-is)
      (loop for each in funcs
            when    (yes-or-no-p (car each))
            collect (cdr each)))))

;; Configuration setup functions

(defmacro* shampoo-fileout-fill (&key conf field provider)
  (let ((selector
         (intern
          (concat "shampoo-fileout-conf-" (symbol-name field)))))
    `(when (null (,selector ,conf))
       (setf (,selector ,conf) ,provider))))

(defun shampoo-fileout-fill-conf (fileout-subject conf)
  (shampoo-fileout-fill
   :conf     conf
   :field    splitby
   :provider (shampoo-fileout-conf-get-splitby fileout-subject))
  (shampoo-fileout-fill
   :conf     conf
   :field    directory
   :provider (shampoo-fileout-conf-get-directory))
  (shampoo-fileout-fill
   :conf     conf
   :field    fproc
   :provider (shampoo-fileout-conf-get-fproc fileout-subject)))

(defun* shampoo-fileout-get-conf (type value)
  (let* ((fileout-subject (cons type value))
         (conf (shampoo-msum
                (shampoo-fileout-script-for fileout-subject)
                (shampoo-fileout-saved-for  fileout-subject)
                (make-shampoo-fileout-conf))))
    (setf (shampoo-fileout-conf-item conf) value)
    (shampoo-fileout-fill-conf fileout-subject conf)
    (when (not (shampoo-fileout-conf-is-loaded conf))
      (shampoo-fileout-try-save fileout-subject conf))
    conf))

(defmacro mkmatcher (type-to-match value-to-match)
  `(lexical-let ((type  ,type-to-match)
                 (value ,value-to-match))
     (lambda (fileout-subject)
       (and (eql   type  (car fileout-subject))
            (equal value (cdr fileout-subject))))))

(defun shampoo-fileout-namespace-match (namespace-name)
  (mkmatcher :namespace namespace-name))

(defun shampoo-fileout-subject->matcher (fileout-subject)
  (let ((type  (car fileout-subject))
        (value (cdr fileout-subject)))
    (mkmatcher type value)))

(defun define-shampoo-fileout (matcher config)
  (setf (shampoo-fileout-conf-is-loaded config) t)
  (pushnew (cons matcher config) *shampoo-fileout-scenarios*))

(defun shampoo-fileout-conf-lookup (fileout-subject items)
  (block root
    (dolist (each items)
      (when (funcall (car each) fileout-subject)
        (return-from root (copy-shampoo-fileout-conf (cdr each)))))
    nil))

(defun shampoo-fileout-script-for (fileout-subject)
  (shampoo-fileout-conf-lookup
   fileout-subject
   *shampoo-fileout-scenarios*))

;; FileOut request producer functions

(defun shampoo-fileout-namespace (default)
  (let ((conf (shampoo-fileout-get-conf
               :namespace
               (shampoo-fileout-ask
                :what    "namespace"
                :from    "*shampoo-namespaces*"
                :default default)))
        (id   (shampoo-give-id)))
    (shampoo-subscribe id (shampoo-save-fileout :config conf))
    (shampoo-send-message
     (shampoo-make-fileout-namespace-rq
      :id    id
      :ns    (shampoo-fileout-conf-item conf)
      :split (shampoo-fileout-conf-splitby conf)))))

(defun shampoo-fileout-class (default)
  (let ((conf (shampoo-fileout-get-conf
               :class
               (shampoo-fileout-ask
                :what    "class"
                :from    "*shampoo-classes*"
                :default default)))
        (id   (shampoo-give-id)))
    (shampoo-subscribe id (shampoo-save-fileout :config conf))
    (shampoo-send-message
     (shampoo-make-fileout-class-rq
      :id     id
      :ns     (shampoo-get-current-namespace)
      :class  (shampoo-fileout-conf-item conf)))))

(defun shampoo-fileout-class-category (default)
  (let ((conf (shampoo-fileout-get-conf
               :category
               (shampoo-fileout-ask
                :what    "category"
                :default default)))
        (id   (shampoo-give-id)))
    (shampoo-subscribe id (shampoo-save-fileout :config conf))
    (shampoo-send-message
     (shampoo-make-fileout-category-rq
      :id    id
      :ns    (shampoo-get-current-namespace)
      :cat   (shampoo-fileout-conf-item conf)
      :split (shampoo-fileout-conf-splitby conf)))))

;; Save/load fileout settings (for session)

(defun shampoo-fileout-try-save (fileout-subject conf)
  (when
      (yes-or-no-p
       "Would you like to use this configuration for the further fileouts? ")
    (let ((stored-conf (copy-shampoo-fileout-conf conf)))
      (setf (shampoo-fileout-conf-is-loaded stored-conf) t)
      (with-~shampoo~
       (pushnew
        (cons (shampoo-fileout-subject->matcher fileout-subject)
              stored-conf)
        (shampoo-current-fileout-configs ~shampoo~))))))

(defun shampoo-fileout-saved-for (fileout-subject)
  (with-~shampoo~
   (shampoo-fileout-conf-lookup
    fileout-subject
    (shampoo-current-fileout-configs ~shampoo~))))

;; Top-level fileout functions

(defun shampoo-fileout-current-namespace ()
  (interactive)
  (shampoo-fileout-namespace (shampoo-get-current-namespace)))

(defun shampoo-fileout-current-class ()
  (interactive)
  (shampoo-fileout-class (shampoo-get-current-class)))

(defun shampoo-fileout-current-class-category ()
  (interactive)
  (shampoo-fileout-class-category
   (shampoo-get-current-class-category)))

(provide 'shampoo-fileout)
 
;; shampoo-fileout.el ends here.
