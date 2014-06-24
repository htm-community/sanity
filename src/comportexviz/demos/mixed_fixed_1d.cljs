(ns comportexviz.demos.mixed-fixed-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [comportexviz.parameters]))

(def bit-width 400)
(def on-bits 25)
(def numb-max 15)
(def numb-domain [0 numb-max])

(def patterns
  [(range 0 5)
   (range 3 9)
   (range 8 12)
   (reverse (range 0 15))])

;; TODO avoid infinite seq, just use a value & transform function

(defn mix-patterns
  "Returns an infinite sequence of sets of numbers."
  [patterns]
  (->> patterns
       (map #(apply concat (repeat %)))
       (apply map (fn [& xs] (set xs)))))

(defn crouching-head-hidden-tail
  "Returns the first element, with the rest in metadata to avoid
   printing an infinite sequence."
  [xs]
  (-> (first xs)
      (with-meta {::next (next xs)})))

;; a function not a value; do not hold on to the head of an infinite seq.
(defn initial-input
  []
  (let [inseq (mix-patterns patterns)]
    (crouching-head-hidden-tail inseq)))

(defn input-transform
  [v]
  (crouching-head-hidden-tail (::next (meta v))))

(def efn
  (enc/superpose-encoder
   (enc/linear-number-encoder bit-width on-bits numb-domain)))

(defn ^:export model
  []
  (let [gen (core/generator (initial-input) input-transform efn
                            {:bit-width bit-width})
        spec (assoc comportexviz.parameters/small
               :input-size bit-width
               :potential-radius (quot bit-width 4))]
    (core/cla-model gen spec)))
