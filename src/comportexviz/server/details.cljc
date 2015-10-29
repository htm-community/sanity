(ns comportexviz.server.details
  (:require [clojure.string :as str]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]))

(defn to-fixed
  [n digits]
  #?(:cljs (.toFixed n digits)
     :clj (format (str "%." digits "f") n)))

(defn detail-text
  [htm prior-htm rgn-id lyr-id col]
  (let [rgn (get-in htm [:regions rgn-id])
        lyr (get rgn lyr-id)
        depth (p/layer-depth lyr)
        in (:input-value htm)
        in-bits (:in-ff-bits (:state lyr))
        in-sbits (:in-stable-ff-bits (:state lyr))]
    (->>
     ["__Selection__"
      (str "* timestep " (p/timestep rgn))
      (str "* column " (or col "nil"))
      ""
      "__Input__"
      (str in)
      (str "(" (count in-bits) " bits, of which " (count in-sbits) " stable)")
      (cond
        (:newly-engaged? (:state lyr)) "= newly engaged"
        (:engaged? (:state lyr)) "= continuing engaged"
        :else "= not engaged")
      ""
      "__Input bits__"
      (str (sort in-bits))
      ""
      "__Active columns__"
      (str (sort (p/active-columns lyr)))
      ""
      "__Bursting columns__"
      (str (sort (p/bursting-columns lyr)))
      ""
      "__Winner cells__"
      (str (sort (p/winner-cells lyr)))
      ""
      "__Proximal learning__"
      (for [seg-up (sort-by :target-id (vals (:proximal-learning (:state lyr))))]
        (str (:target-id seg-up) " " (dissoc seg-up :target-id :operation)))
      ""
      "__Distal learning__"
      (for [seg-up (sort-by :target-id (vals (:distal-learning (:state lyr))))]
        (str (:target-id seg-up) " " (dissoc seg-up :target-id :operation)))
      ""
      "__Distal punishments__"
      (for [seg-up (sort-by :target-id (:distal-punishments (:state lyr)))]
        (str (:target-id seg-up)))
      ""
      "__TP excitation__"
      (str (sort (:temporal-pooling-exc (:state lyr))))
      ""
      (if (and col prior-htm)
        (let [p-lyr (get-in prior-htm [:regions rgn-id lyr-id])
              p-prox-sg (:proximal-sg p-lyr)
              p-distal-sg (:distal-sg p-lyr)
              d-pcon (:perm-connected (:distal (p/params p-lyr)))
              ff-pcon (:perm-connected (:proximal (p/params p-lyr)))
              bits (:in-ff-bits (:state lyr))
              sig-bits (:in-stable-ff-bits (:state lyr))
              d-bits (:on-bits (:prior-distal-state lyr))
              d-lc-bits (:on-lc-bits (:prior-distal-state lyr))
              ]
          ["__Column overlap__"
           (str (get (:col-overlaps (:state lyr)) [col 0]))
           ""
           "__Selected column__"
           "__Connected ff-synapses__"
           (for [[si syns] (map-indexed vector (p/cell-segments p-prox-sg [col 0]))
                 :when (seq syns)]
             [(str "FF segment " si)
              (for [[id p] (sort syns)
                    :let [[src-k src-i] (core/source-of-incoming-bit htm rgn-id id :ff-deps)
                          src-rgn (get-in htm [:regions src-k])
                          src-id (if src-rgn (p/source-of-bit src-rgn src-i) src-i)
                          ]]
                (str "  " src-k " " src-id
                     (if (>= p ff-pcon) " :=> " " :.: ")
                     (to-fixed p 2)
                     (if (contains? sig-bits id) " S"
                         (if (contains? bits id) " A"))))])
           "__Cells and their distal dendrite segments__"
           (for [ci (range (p/layer-depth lyr))
                 :let [segs (p/cell-segments p-distal-sg [col ci])]]
             [(str "CELL " ci)
              (str (count segs) " = " (map count segs))
              (for [[si syns] (map-indexed vector segs)]
                [(str "  SEGMENT " si)
                 (for [[id p] (sort syns)
                       :let [[src-k _ src-i] (core/source-of-distal-bit htm rgn-id lyr-id id)
                             src-rgn (get-in htm [:regions src-k])
                             src-id (if src-rgn (p/source-of-bit src-rgn src-i) src-i)]]
                   (str "    " src-k " " src-id
                        (if (>= p d-pcon) " :=> " " :.: ")
                        (to-fixed p 2)
                        (if (contains? d-lc-bits id) " L"
                            (if (contains? d-bits id) " A"))))])
              ])
           ]))
      ""
      "__spec__"
      (map str (sort (p/params rgn)))]
     (flatten)
     (interpose \newline)
     (apply str))))
