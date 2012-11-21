;;; shampoo-faces.el --- Shampoo custom faces
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(defface shampoo-selected-list-item
  '((((class color) (min-colors 88))
     :background "royal blue" :foreground "white")
    (((class color) (min-colors 8))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Selected list item face"
  :group 'basic-faces)

(defface shampoo-smalltalk-version
  '((((class color) (min-colors 88) (background light))
     :foreground "dark green")
    (((class color) (min-colors 88) (background dark))
     :foreground "pale green")
    (((class color) (min-colors 8))
     :foreground "magenta"))
  "Smalltalk system version face"
  :group 'basic-faces)

(defface shampoo-method-name
  '((((background light))
     :foreground "black" :weight black)
    (((background dark))
     :foreground "white" :weight black))
  "Smalltalk system version face"
  :group 'basic-faces)

(defun shampoo-set-line-face (facename)
  (let ((buffer-read-only nil))
    (add-text-properties (line-beginning-position)
                         (line-end-position)
                         `(face ,facename))))

(defun shampoo-set-str-face (str facename)
  (add-text-properties 0 (length str) `(face ,facename) str)
  str)

(defun shampoo-reset-buffer-faces ()
  (let ((buffer-read-only nil))
    (remove-text-properties (point-min) (point-max) `(face nil))))

(provide 'shampoo-faces)

;;; shampoo-faces.el ends here.