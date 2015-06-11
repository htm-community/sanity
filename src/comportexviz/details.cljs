(ns comportexviz.details
  (:require [clojure.string :as str]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]))

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
      "__Bursting columns__"
      (str (sort (p/bursting-columns lyr)))
      ""
      "__Learnable cells__"
      (str (sort (p/learnable-cells lyr)))
      ""
      "__Proximal learning__"
      (str (sort (:proximal-learning (:state lyr))))
      ""
      "__Distal learning__"
      (str (sort (:distal-learning (:state lyr))))
      ""
      "__TP cells__"
      (str (sort (p/temporal-pooling-cells lyr)))
      ""
      "__TP excitation__"
      (str (sort (:temporal-pooling-exc (:state lyr))))
      ""
      (if col
        (let [p-lyr (get-in prior-htm [:regions rgn-id lyr-id])
              p-prox-sg (:proximal-sg p-lyr)
              p-distal-sg (:distal-sg p-lyr)
              ac (p/active-cells p-lyr)
              lc (or (p/learnable-cells p-lyr) #{})
              pcon (:distal-perm-connected (p/params p-lyr))
              bits (:in-ff-bits (:state lyr))
              sig-bits (:in-stable-ff-bits (:state lyr))
              ]
          ["__Column overlap__"
           (str (get (:col-overlaps (:state lyr)) [col 0]))
           ""
           "__Distal LC bits prev__"
           (str (:distal-lc-bits (:prior-distal-state lyr)))
           ""
           "__Distal exc prev__"
           (str (sort (:distal-exc (:prior-distal-state lyr))))
           ""
           "__Selected column__"
           "__Connected ff-synapses__"
           (for [[si syns] (map-indexed vector (p/cell-segments p-prox-sg [col 0]))
                 :when (seq syns)]
             [(str "FF segment " si)
              (for [[id p] (sort syns)]
                (str "  " id " :=> "
                     (.toFixed p 2)
                     (if (get sig-bits id) " S")
                     (if (get bits id) (str " A "
                                            (core/source-of-incoming-bit htm rgn-id id)
                                            ))))])
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
                        (.toFixed p 2)
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
