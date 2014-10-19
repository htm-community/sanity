(ns comportexviz.cortical-io
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util]
            [cljs-http.client :as http]
            [clojure.string :as str]
            [cljs.core.async :refer [>! <! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def base-uri "http://api.cortical.io/rest")

(def query-params {:retina_name "en_associative"})

(defn get-fingerprint
  [api-key term]
  (http/post (str base-uri "/expressions")
             {:query-params query-params
              :json-params {:term term}
              :with-credentials? false
              :headers {"api-key" api-key}}))

(defn get-similar-terms
  [api-key bits max-n]
  (http/post (str base-uri "/expressions/similar_terms")
             {:query-params (assoc query-params
                              :get_fingerprint true
                              :max_results max-n)
              :json-params {:positions bits}
              :with-credentials? false
              :headers {"api-key" api-key}}))

(defn apply-offset
  [xs offset]
  (into (empty xs) (map #(+ % offset))))

(defn cortical-io-encoder
  [api-key min-votes terms]
  (let [topo (topology/make-topology [128 128])
        cached-bits (atom {})
        gen-rand (fn []
                   (set (repeatedly (* (p/size topo) 0.02)
                                    #(util/rand-int 0 (dec (p/size topo))))))]
    ;; store the special token "." as a random SDR
    (swap! cached-bits assoc "." (gen-rand))
    ;; kick off the process to load the fingerprints
    (go
      (doseq [term (->> terms
                        (remove #{"."})
                        (map str/lower-case))]
        (println "requesting fingerprint for:" term)
        (swap! cached-bits assoc term
               (let [result (<! (get-fingerprint api-key term))]
                 (if (:success result)
                   (set (get-in result [:body :positions]))
                   (do (println result)
                       (gen-rand)))))
        (<! (timeout 500))))
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ offset term]
        (if (seq term)
          (cond->
           (get @cached-bits (str/lower-case term))
           (not (zero? offset)) (apply-offset offset))
          #{}))
      (decode
        [_ bit-votes n]
        {:channel
         (go
           (let [bits (keep (fn [[i votes]]
                              (when (>= votes min-votes) i))
                            bit-votes)]
             (if (empty? bits)
               []
               (let [total-votes (apply + (vals bit-votes))
                     result (<! (get-similar-terms api-key bits n))]
                 (println "requested similar terms.")
                 (if (:success result)
                   (->> (:body result)
                        (cons {:term "."
                               :positions (get @cached-bits ".")})
                        (map (fn [item]
                               (let [x-bits (set (:positions item))
                                     o-votes (select-keys bit-votes x-bits)
                                     total-o-votes (apply + (vals o-votes))
                                     o-bits (keys o-votes)]
                                 {:value (:term item)
                                  :bit-coverage (/ (count o-bits)
                                                   (count x-bits))
                                  :bit-precision (/ (count o-bits)
                                                    (count bit-votes))
                                  :votes-frac (/ total-o-votes
                                                 total-votes)
                                  :votes-per-bit (/ total-o-votes
                                                    (count x-bits))}))
                             )
                        ;(filter (comp pos? :votes-frac))
                        ;(sort-by (juxt :votes-frac :bit-coverage :bit-precision) >)
                        (take (inc n)))
                   (println result))))))}))))
