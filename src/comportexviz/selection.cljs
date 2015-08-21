(ns comportexviz.selection)

(def blank-selection [])

(defn input
  [sel1]
  (let [[t a1] (:path sel1)]
    (when (= t :inputs)
      a1)))

(defn layer
  [sel1]
  (let [[t a1 a2] (:path sel1)]
    (when (= t :regions)
      [a1 a2])))

(defn clear
  [sel]
  (conj (empty sel)
        (select-keys (peek sel) [:dt :path :model-id])))
