(in-package #:ucl)

(export '(prompt-read
          prompt-read-integer))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-integer (prompt)
  (or (parse-integer (prompt-read prompt) :junk-allowed t) 0))
