(use 'do.no.number23.commons-lib.io)
(use 'de.no.number23.commons-lib.util)

(defmacro kw
  "查询当前所有ns中含特定字符串的函数，如: (kw -index)
  from http://www.newsmth.net/bbscon.php?bid=579&id=25982"
  [s] `(filter #(>= (.indexOf (str %) (name '~s)) 0)
               (sort (keys (mapcat ns-publics (all-ns))))))

;; (defmacro unless [& args] `(when-not ~@args))
;; (clojure.contrib.def/defalias unless when-not)
(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

