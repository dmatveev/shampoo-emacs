;;; shampoo-modes.el --- Shampoo FileOut routines
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'cl)
(require 'shampoo-state)
(require 'shampoo-requests)
(require 'shampoo-response)
(require 'shampoo-utils)

(defstruct shampoo-fileout-conf item splitby directory)

(defun shampoo-fileout-build-filename (name conf)
  (format
   "%s%s.st"
   (file-name-as-directory (shampoo-fileout-conf-directory conf))
   name
   ".st"))

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

(defun* shampoo-fileout-get-conf
  (&key item-name items-buffer default-value no-split)
  (let* ((item-prompt (format "File out %s: " item-name))
         (item (completing-read item-prompt
                                (shampoo-buffer-lines items-buffer)
                                nil t default-value))
         (by   (when (not no-split)
                 (completing-read "Organize source code files by: "
                                  '("class" "category")
                                  nil t "class")))
         (to   (read-directory-name "Store files into directory: ")))
    (make-shampoo-fileout-conf :item item :splitby by :directory to)))

(defun* shampoo-save-fileout (&key config)
  (lexical-let ((conf config))
    (lambda (response)
      (let ((file (shampoo-response-attr 'class response)))
        (when (null file)
          (setq file (shampoo-response-attr 'category response)))
        (let ((path (shampoo-fileout-build-filename file conf)))
          (with-temp-buffer
            (insert (shampoo-response-enclosed-string response))
            (write-region nil nil path))))
      (when (shampoo-response-is-last-in-sequence response)
        (message "Shampoo: file out complete")))))

(defun shampoo-fileout-namespace (default)
  (let ((conf (shampoo-fileout-get-conf
               :item-name     "namespace"
               :items-buffer  "*shampoo-namespaces*"
               :default-value default))
        (id   (shampoo-give-id)))
    (shampoo-subscribe id (shampoo-save-fileout :config conf))
    (shampoo-send-message
     (shampoo-make-fileout-namespace-rq
      :id    id
      :ns    (shampoo-fileout-conf-item conf)
      :split (shampoo-fileout-conf-splitby conf)))))

(defun shampoo-fileout-class (default)
  (let ((conf (shampoo-fileout-get-conf
               :item-name     "class"
               :items-buffer  "*shampoo-classes*"
               :default-value default
               :no-split      t))
        (id   (shampoo-give-id)))
    (shampoo-subscribe id (shampoo-save-fileout :config conf))
    (shampoo-send-message
     (shampoo-make-fileout-class-rq
      :id     id
      :ns     (shampoo-get-current-namespace)
      :class  (shampoo-fileout-conf-item conf)))))

(defun shampoo-fileout-class-category (category)
  )

(provide 'shampoo-fileout)