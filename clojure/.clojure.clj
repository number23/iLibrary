(defn exit [] (System/exit 0))
(defn quit [] (System/exit 0))
(defmacro kw
  "查询当前所有ns中含特定字符串的函数，如: (kw -index)
  from http://www.newsmth.net/bbscon.php?bid=579&id=25982"
  [s] `(filter #(>= (.indexOf (str %) (name '~s)) 0)
               (sort (keys (mapcat ns-publics (all-ns))))))
(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))
