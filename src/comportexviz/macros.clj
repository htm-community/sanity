(ns comportexviz.macros)

(defmacro with-cache
  "Returns the value of `expr`, storing it also in cache (atom) under
   key `k`. The value in cache key `:opts` is compared to the supplied
   `opts` argument. If the values under keys `opts-ks` are equal, then
   the cached value is returned and the expression is not evaluated."
  [cache k opts opts-ks expr]
  `(->
    (if (= (select-keys ~opts ~opts-ks)
           (select-keys (:opts (deref ~cache)) ~opts-ks))
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
