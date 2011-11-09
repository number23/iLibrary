(in-package #:ucl)

(export '(print-list
          read-lines))

(defun print-list (lst)
    (dolist (x lst) (print x)))

(defun read-lines (file)
  (let (lines)
    (with-open-file (stream file)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (push line lines)))
    (reverse lines)))
