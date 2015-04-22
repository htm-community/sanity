(ns comportexviz.helpers
  (:require [goog.dom]
            [goog.dom.classes]))

(defn with-ui-loading-message
  [f]
  (let [el (goog.dom/getElement "loading-message")]
     (goog.dom.classes/add el "show")
     ;; need a timeout to allow redraw to show loading message
     (js/setTimeout (fn []
                      (try
                        (f)
                        (finally
                          (goog.dom.classes/remove el "show"))))
                    100)))
