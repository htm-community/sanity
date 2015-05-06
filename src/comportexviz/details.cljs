(ns comportexviz.details
  (:require [clojure.string :as str]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [goog.string :as gstring]
            [goog.string.format]))

(defn detail-text
  [htm
   prior-htm
   {dt :dt
    rgn-id :region
    lyr-id :layer
    col :col
    :as selection}]
  (let [rgn (get-in htm [:regions rgn-id])
        lyr (get rgn lyr-id)
        depth (p/layer-depth lyr)
        inp (first (core/input-seq htm))
        in (:value inp)
        bits (p/bits-value inp)]
    (->>
     ["__Selection__"
      (str "* timestep " (p/timestep rgn)
           " (delay " dt ")")
      (str "* column " (or col "nil"))
      ""
      "__Input__"
      (str in " (" (count bits) " bits)")
      ""
      "__Input bits__"
      (str (sort bits))
      ""
      "__Active columns__"
      (str (sort (p/active-columns lyr)))
      ""
      "__Active cells__"
      (str (sort (p/active-cells lyr)))
      ""
      "__Learnable cells__"
      (str (sort (p/learnable-cells lyr)))
      ""
      "__Learning segments__"
      (str (sort (:learn-segments (:state lyr))))
      ""
      "__TP cells__"
      (str (sort (p/temporal-pooling-cells lyr)))
      ""
      "__Slow proximal excitation__"
      (str (sort (:proximal-slow-exc (:state lyr))))
      ""
      "__Predicted cells__"
      (str (sort (p/predictive-cells lyr)))
      ""
      (if col
        (let [p-lyr (get-in prior-htm [:regions rgn-id lyr-id])
              p-prox-sg (:proximal-sg p-lyr)
              p-distal-sg (:distal-sg p-lyr)
              ac (p/active-cells p-lyr)
              lc (or (p/learnable-cells p-lyr) #{})
              pcon (:distal-perm-connected (p/params p-lyr))
              ;; TODO
              bits #{}
              sig-bits #{}
              ]
          ["__Active cells prev__"
           (str (sort ac))
           ""
           "__Learn cells prev__"
           (str (sort lc))
           ""
           "__Distal LC bits prev__"
           (str (:distal-lc-bits (:prior-distal-state lyr)))
           ""
           "__Distal LC bits__"
           (str (:distal-lc-bits (:distal-state lyr)))
           ""
           "__Distal bits__"
           (str (:distal-bits (:distal-state lyr)))
           ""
           "__Predicted cells prev__"
           (str (sort (p/predictive-cells p-lyr)))
           ""
           "__Selected column__"
           "__Connected ff-synapses__"
           (let [syns (p/in-synapses p-prox-sg col)]
             (for [[id p] (sort syns)]
               (str "  " id " :=> "
                    (gstring/format "%.2f" p)
                    (if (sig-bits id) " S")
                    (if (bits id) (str " A "
                                       ;(p/source-of-incoming-bit)
                                       )))))
           "__Cells and their Dendrite segments__"
           (for [ci (range (p/layer-depth lyr))
                 :let [segs (p/cell-segments p-distal-sg [col ci])]]
             [(str "CELL " ci)
              (str (count segs) " = " (map count segs))
              #_(str "Distal excitation from this cell: "
                   (p/targets-connected-from p-distal-sg (+ ci (* depth col)))) ;; TODO cell->id
              (for [[si syns] (map-indexed vector segs)]
                [(str "  SEGMENT " si)
                 (for [[id p] (sort syns)]
                   (str "  " id
                        (if (>= p pcon) " :=> " " :.: ")
                        (gstring/format "%.2f" p)
                        (if (lc id) " L"
                            (if (ac id) " A"))))])
              ])
           ]))
      ""
      "__spec__"
      (map str (sort (p/params rgn)))]
     (flatten)
     (interpose \newline)
     (apply str))))
