(ns comportexviz.macros)

(defmacro with-cache
  "Returns the value of `expr`, storing it also in cache ref under key
   `k`. If the cache `:opts` already contains a value in key `opts-k
   which is equal to the same key in `opts`, then the cached value is
   returned and the expression is not evaluated."
  [cache k opts opts-k expr]
  `(->
    (if (= (~opts-k ~opts) (~opts-k (:opts (deref ~cache))))
      (deref ~cache)
      (swap! ~cache assoc ~k ~expr))
    (get ~k)))

(defmacro with-ui-loading-message
  [& body]
  `(let [el# (c2.dom/->dom "#comportex-loading")]
     (c2.dom/style el# {:display "block"})
     ;; need a timeout to allow redraw to show loading message
     (js/setTimeout (fn []
                      (try
                        ~@body)
                      (c2.dom/style el# {:display "none"}))
                    100)))
