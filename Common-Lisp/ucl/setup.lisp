;;;; utility common lisp
;;;;
;;;; how to setup? add into CL init file:
;;;; (let ((ucl-init (merge-pathnames "hgroot/bitbucket.org/utilities/Common-Lisp/ucl/setup.lisp"
;;;;                                  (user-homedir-pathname))))
;;;;  (when (probe-file ucl-init)
;;;;    (load ucl-init)))

(defpackage #:ucl (:use #:cl))

(in-package #:ucl)

(unless *load-truename*
  (error "This file must be LOADed to set up."))

(defparameter *ucl-home*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

;;(defun ucl-merge (pathname)
;;  "Return PATHNAME merged with the base directory."
;;  (merge-pathnames pathname *ucl-home*))
;;
;;(load (ucl-merge "ucl-01.lisp"))

(dolist (f (directory (concatenate 'string
                                   (directory-namestring *ucl-home*)
                                   "ucl*.lisp")))
  (load f))
