;; slime-autodoc
(add-hook
 'slime-mode-hook
 (lambda ()
   (if (eq major-mode 'clojure-mode)
       (slime-autodoc-mode 0)
     (slime-autodoc-mode 1))))

(add-hook
 'slime-connected-hook
 (lambda ()
   (if (string= (slime-lisp-implementation-type) "Clojure")
       (setq slime-use-autodoc-mode nil)
     (setq slime-use-autodoc-mode t))))

(add-hook
 'slime-repl-mode-hook
 (lambda ()
   (if (string= (slime-lisp-implementation-type) "Clojure")
       (progn (setq slime-use-autodoc-mode nil)
              (slime-autodoc-mode 0))
     (progn (setq slime-use-autodoc-mode t)
            (slime-autodoc-mode 1)))))

(provide 'c-slime-autodoc)
