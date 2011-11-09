;;;; from http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
;;;; (get-unix-time) ==> 1320139343
;;;; date +%s        ==> 1320139343

(in-package #:ucl)

(export '(universal-to-unix-time
          unix-to-universal-time
          get-unix-time))

(defparameter *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

;;;; from http://cl-cookbook.sourceforge.net/dates_and_times.html
(defun day-of-week (day month year)
  "Returns the day of the week as an integer.
Sunday is 0. Works for years after 1752."
  (let ((offset '(0 3 2 5 0 3 5 1 4 6 2 4)))
    (when (< month 3)
      (decf year 1))
    (mod
     (truncate (+ year
                  (/ year 4)
                  (/ (- year)
                     100)
                  (/ year 400)
                  (nth (1- month) offset)
                  day
                  -1))
     7)))
