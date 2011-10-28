(in-package #:ucl)

(export '(print-list))

(defun print-list (lst)
    (dolist (x lst) (print x)))
