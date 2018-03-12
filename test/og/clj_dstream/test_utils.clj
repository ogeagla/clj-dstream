(ns og.clj-dstream.test-utils
  (:require [og.clj-dstream.clustering.core :as core]))

(def TWO_PI (* 2 (Math/PI)))

(defn cluster-count [state]
  (->> state
       ::core/grid-cells
       vals
       (map ::core/cluster)
       (remove #(not (core/is-cluster? %)))
       distinct
       count))

(defn label-count [state label]
  (->> state
       ::core/grid-cells
       vals
       (map ::core/label)
       (filter #(= label %))
       count))

(defn sample-ring-2d [r-inner r-outer
                      & {:keys [offsets] :or {offsets [0.0 0.0]}}]
  "Random [x y] inside a ring centered
  about origin with minor/major radii"
  (let [r     (+ r-inner
                 (rand (- r-outer r-inner)))
        theta (rand TWO_PI)
        x     (* r (Math/cos theta))
        y     (* r (Math/sin theta))]
    [(+ x (first offsets)) (+ y (second offsets))]))


(defn sample-ring-arc-2d [theta-start theta-end r-inner r-outer
                          & {:keys [offsets] :or {offsets [0.0 0.0]}}]
  "Random [x y] inside a ring centered
  about origin with minor/major radii
  and with degree range"
  (let [r     (+ r-inner
                 (rand (- r-outer r-inner)))
        theta (+ theta-start (rand (- theta-end theta-start)))
        x     (* r (Math/cos theta))
        y     (* r (Math/sin theta))]
    [(+ x (first offsets)) (+ y (second offsets))]))

(defn sample-circle-2d [r & {:keys [offsets] :or {offsets [0.0 0.0]}}]
  (sample-ring-2d 0 r :offsets offsets))

(defn sample-rect-2d [w h & {:keys [offsets] :or {offsets [0.0 0.0]}}]
  (let [rand-x (+ (first offsets) (rand w))
        rand-y (+ (second offsets) (rand h))]
    [rand-x rand-y]))

(defn sample-rect [sizes offsets]
  (vec
    (map-indexed
      (fn [idx offset]
        (+ offset
           (rand (get sizes idx))))
      offsets)))