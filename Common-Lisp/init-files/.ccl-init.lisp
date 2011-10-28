;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; ucl
(let ((ucl-init (merge-pathnames "iLibrary/Common-Lisp/ucl/setup.lisp"
                                 (user-homedir-pathname))))
  (when (probe-file ucl-init)
    (load ucl-init)))
