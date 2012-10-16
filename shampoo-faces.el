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
  "Selected list item face")

(defun shampoo-set-line-face (facename)
  (let ((buffer-read-only nil))
    (add-text-properties (line-beginning-position)
                         (line-end-position)
                         `(face ,facename))))

(defun shampoo-reset-buffer-faces ()
  (let ((buffer-read-only nil))
    (remove-text-properties (point-min)
                            (point-max)
                            `(face nil))))

(provide 'shampoo-faces)

;;; shampoo-faces.el ends here.