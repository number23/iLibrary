(defn exit [] (System/exit 0))
(defn quit [] (System/exit 0))

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

;;; from http://thinkrelevance.com/blog/2008/09/16/pcl-clojure-chapter-3.html
(defn prompt-read [prompt]
  (print (format "%s: " prompt))
  (flush)
  (read-line))

(defn y-or-n-p [prompt]
  (= "y"
     (loop []
       (or
        (re-matches #"[yn]" (.toLowerCase (prompt-read prompt)))
        (recur)))))
