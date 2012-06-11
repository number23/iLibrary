(use '[de.no.number23.commons-lib io util])

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

;; from <<Clojure Programming>>, P292.
(defn scaffold
  "Given an interface, returns a 'hollow' body suitable for use with `deftype`."
  [interface]
  (doseq [[iface methods] (->> interface
                               .getMethods
                               (map #(vector (.getName (.getDeclaringClass %))
                                             (symbol (.getName %))
                                             (count (.getParameterTypes %))))
                               (group-by first))]
    (println (str " " iface))
    (doseq [[_ name argcount] methods]
      (println
       (str "    "
            (list name (into '[this] (take argcount (repeatedly gensym)))))))))
