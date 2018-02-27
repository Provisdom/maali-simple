(ns provisdom.simple.app
  (:require [provisdom.maali.rules :refer-macros [defsession] :as rules]
            [provisdom.simple.rules :as simple]))

(enable-console-print!)

(defn foo
  []
  (.log js/console "OGGY"))

(foo)
