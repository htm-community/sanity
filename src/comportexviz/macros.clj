(ns org.numenta.sanity.macros)

(defmacro with-cache
  "Returns the value of `expr`, storing it also in cache (atom) under
   key `k`. The value in cache key `[:opts k]` is compared to the
   supplied `opts` argument. If the values under keys `opts-ks` are
   equal, then the cached value is returned and the expression is not
   evaluated."
  [cache k opts opts-ks expr]
  `(->
    (if (= (select-keys ~opts
                        ~opts-ks)
           (select-keys (get (deref ~cache) [:opts ~k])
                        ~opts-ks))
      (deref ~cache)
      (swap! ~cache assoc ~k ~expr
             [:opts ~k] ~opts))
    (get ~k)))

(defmacro with-ui-loading-message
  [& body]
  `(org.numenta.sanity.helpers/with-ui-loading-message
     (fn [] ~@body)))
