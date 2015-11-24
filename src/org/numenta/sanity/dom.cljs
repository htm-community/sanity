(ns org.numenta.sanity.dom)

(defn get-bounding-page-rect [el]
  (let [[left top] (loop [el el
                          x 0
                          y 0]
                     (if el
                       (let [s (js/getComputedStyle el)
                             ;; offsetTop, etc. don't include the border width
                             ;; for positioned elements.
                             include-border? (not= (.-position s) "static")]
                         (recur (.-offsetParent el)
                                (-> x
                                    (+ (.-offsetLeft el))
                                    (cond-> include-border?
                                            (+ (-> (.-borderLeftWidth s)
                                                   js/parseInt)))
                                    (cond-> (not= el js/document.body)
                                            (- (.-scrollLeft el))))
                                (-> y
                                    (+ (.-offsetTop el))
                                    (cond-> include-border?
                                            (+ (-> (.-borderTopWidth s)
                                                   js/parseInt)))
                                    (cond-> (not= el js/document.body)
                                            (- (.-scrollTop el))))))
                       [x y]))]
    [left top (+ left (.-offsetWidth el)) (+ top (.-offsetHeight el))]))

(defn within-element? [evt el]
  (let [[left top right bottom] (get-bounding-page-rect el)]
    (and (>= (.-pageX evt) left)
         (< (.-pageX evt) right)
         (>= (.-pageY evt) top)
         (< (.-pageY evt) bottom))))

(defn nonzero-number? [v]
  (if (and (number? v)
           (not (zero? v)))
    v
    false))

;; Ported from jQuery.
(defn page-x [evt]
  (or (.-pageX evt)
      (let [doc js/document.documentElement
            body js/document.body]
        (+ (.-clientX evt)
           (- (or (nonzero-number? (and doc (.-scrollLeft doc)))
                  (nonzero-number? (and body (.-scrollLeft body)))
                  0)
              (or (nonzero-number? (and doc (.-clientLeft doc)))
                  (nonzero-number? (and body (.-clientLeft body)))
                  0))))))

;; Ported from jQuery.
(defn page-y [evt]
  (or (.-pageY evt)
      (let [doc js/document.documentElement
            body js/document.body]
        (+ (.-clientY evt)
           (- (or (nonzero-number? (and doc (.-scrollTop doc)))
                  (nonzero-number? (and body (.-scrollTop body)))
                  0)
              (or (nonzero-number? (and doc (.-clientTop doc)))
                  (nonzero-number? (and body (.-clientTop body)))
                  0))))))

(defn offset-from [evt el]
  (let [[left top right bottom] (get-bounding-page-rect el)]
    {:x (- (page-x evt) left)
     :y (- (page-y evt) top)}))

(defn offset-from-target [evt]
  (offset-from evt (-> evt .-target)))
