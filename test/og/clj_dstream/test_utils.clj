(ns og.clj-dstream.test-utils
  (:require [og.clj-dstream.core :as core]))

(defn cluster-count [state]
  (->> state
       ::core/grid-cells
       vals
       (map ::core/cluster)
       (remove #(= "NO_CLASS" %))
       distinct
       count))

(defn label-count [state label]
  (->> state
       ::core/grid-cells
       vals
       (map ::core/label)
       (filter #(= label %))
       count))