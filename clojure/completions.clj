(use '[clojure pprint repl])
(use '[clojure.java shell browse javadoc jdbc])
(load-file (str (System/getenv "HOME") "/.clojure.clj"))

(def completions (mapcat (comp keys ns-publics) (all-ns)))
(with-open [f (java.io.BufferedWriter. (java.io.FileWriter. (str (System/getenv "HOME") "/.clj_completions")))]
    (.write f (apply str (interleave completions (repeat "\n")))))
