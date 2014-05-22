(ns comportexviz.mq
  (:require [cljs.core.async :refer [chan <! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def sim-channel (chan))
