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
