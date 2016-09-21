(ns org.numenta.sanity.comportex.details
  (:require [clojure.string :as str]
            [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.layer.tools :as layertools]))

(defn to-fixed
  [n digits]
  #?(:cljs (.toFixed n digits)
     :clj (format (str "%." digits "f") n)))

(defn detail-text
  [htm prior-htm lyr-id col]
  (let [lyr (get-in htm [:layers lyr-id])
        info (cx/layer-state lyr)
        in (:input-value htm)
        in-bits (-> lyr :active-state :in-ff-signal :bits set)
        in-sbits (-> lyr :active-state :in-ff-signal ::layer/stable-bits set)]
    (->>
     ["__Selection__"
      (str "* timestep " (cx/timestep lyr))
      (str "* column " (or col "nil"))
      ""
      "__Input__"
      (str in)
      (str "(" (count in-bits) " bits, of which " (count in-sbits) " stable)")
      ""
      "__Input bits__"
      (str (sort in-bits))
      ""
      "__Active columns__"
      (str (sort (:active-columns info)))
      ""
      "__Bursting columns__"
      (str (sort (:bursting-columns info)))
      ""
      "__Winner cells__"
      (str (sort (:winner-cells info)))
      ""
      "__Proximal learning__"
      (for [seg-up (->> lyr :learn-state :learning :proximal vals (sort-by :target-id))]
        (str (:target-id seg-up) " " (dissoc seg-up :target-id :operation)))
      ""
      "__Distal learning__"
      (for [seg-up (->> lyr :learn-state :learning :distal vals (sort-by :target-id))]
        (str (:target-id seg-up) " " (dissoc seg-up :target-id :operation)))
      ""
      "__Distal punishments__"
      (for [seg-up (->> lyr :learn-state :punishments :distal (sort-by :target-id))]
        (str (:target-id seg-up)))
      ""
      "__Stable cells buffer__"
      (str (seq (:stable-cells-buffer (:active-state lyr))))
      ""
      (if (and col prior-htm)
        (let [p-lyr (get-in prior-htm [:layers lyr-id])
              p-prox-sg (:proximal-sg p-lyr)
              p-distal-sg (:distal-sg p-lyr)
              d-pcon (:perm-connected (:distal (cx/params p-lyr)))
              ff-pcon (:perm-connected (:proximal (cx/params p-lyr)))
              d-bits (:active-bits (:prior-distal-state lyr))
              d-lbits (:learnable-bits (:prior-distal-state lyr))]

          ["__Column overlap__"
           (str (get (:col-overlaps (:active-state lyr)) [col 0]))
           ""
           "__Selected column__"
           "__Connected ff-synapses__"
           (for [[si syns] (map-indexed vector (syn/cell-segments p-prox-sg [col 0]))
                 :when (seq syns)]
             [(str "FF segment " si)
              (for [[i p] (sort syns)
                    :let [[src-k src-i] (cx/source-of-incoming-bit
                                         htm lyr-id i :ff-deps)]]
                (str "  " src-k " " src-i
                     (if (>= p ff-pcon) " :=> " " :.: ")
                     (to-fixed p 2)
                     (if (contains? in-sbits i) " S"
                         (if (contains? in-bits i) " A"))))])
           "__Cells and their distal dendrite segments__"
           (for [ci (range (:depth (cx/params lyr)))
                 :let [segs (syn/cell-segments p-distal-sg [col ci])]]
             [(str "CELL " ci)
              (str (count segs) " = " (map count segs))
              (for [[si syns] (map-indexed vector segs)]
                [(str "  SEGMENT " si)
                 (for [[i p] (sort syns)
                       :let [[src-k src-i] (layertools/source-of-distal-bit
                                            htm lyr-id i)]]
                   (str "    " src-k " " src-i
                        (if (>= p d-pcon) " :=> " " :.: ")
                        (to-fixed p 2)
                        (if (contains? d-lbits i) " L"
                            (if (contains? d-bits i) " A"))))])])]))

      ""
      "__params__"
      (map str (sort (cx/params lyr)))]
     (flatten)
     (interpose \newline)
     (apply str))))
