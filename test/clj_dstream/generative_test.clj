(ns clj-dstream.generative-test
  (:require [clojure.test :refer :all]
            [clj-dstream.core :as core]
            [clj-dstream.test-utils :as test-utils]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.core.vector :as v]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]
            [thi.ng.math.simplexnoise :as n]
            [thi.ng.color.gradients :as grad]
            [think.tsne.core :as tsne]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.random :as random]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))


(defn get-random-sample [method properties & {:keys [centroids]}]
  ;;TODO make this sample some actua; distributions....
  (if centroids
    (let [the-centroid (get centroids (rand-int (count centroids)))]
      (vec
        (map-indexed (fn [idx {:keys [::core/domain-start ::core/domain-end]}]
                       (max domain-start (min domain-end (+ domain-start

                                                            ;(rand (- domain-end domain-start))
                                                            (+ (get the-centroid idx) (/ (first (random/sample-normal 1)) 10.0))
                                                            ))))
                     (::core/phase-space properties))))
    (case method
      :normal (vec
                (map-indexed (fn [idx {:keys [::core/domain-start ::core/domain-end]}]
                               (max domain-start (min domain-end (first (random/sample-normal 1)))))
                             (::core/phase-space properties)))
      :unifrom
      (vec
        (map-indexed (fn [idx {:keys [::core/domain-start ::core/domain-end]}]
                       (max domain-start (min domain-end (+ domain-start (rand (- domain-end domain-start))))))
                     (::core/phase-space properties))))
    ))


(def test-matrix
  (->> (for [y (range 10) x (range 50)] (n/noise2 (* x 0.1) (* y 0.25)))
       (viz/matrix-2d 50 10)))

(defn grids-2d->heatmap-vec [grid-cells props]
  (let [phase-space (::core/phase-space props)
        d1          (first phase-space)
        d2          (second phase-space)
        rows        (int (/ (- (::core/domain-end d1) (::core/domain-start d1)) (::core/domain-interval d1)))
        cols        (int (/ (- (::core/domain-end d2) (::core/domain-start d2)) (::core/domain-interval d2)))]

    (println "rows, cols: " rows cols)

    (let [vecs
          (for [c (range cols) r (range rows)]
                 (let [num (or (::core/density-at-last-update (get grid-cells [r c]))
                               0.0)]
                   num
                   ))]
      (->>
        vecs
        (viz/matrix-2d rows cols)
        ))

    ))

(defn heatmap-spec
  [id the-matrix]
  {:matrix        the-matrix
   :value-domain  (viz/value-domain-bounds the-matrix)
   :palette       (->> id (grad/cosine-schemes) (apply grad/cosine-gradient 100))
   :palette-scale viz/linear-scale
   :layout        viz/svg-heatmap})

(defn cartesian-viz
  [prefix id the-matrix & [opts]]
  (clojure.pprint/pprint the-matrix)
  (->> {:x-axis (viz/linear-axis
                  {:domain [0 50]
                   :range  [50 550]
                   :major  10
                   :minor  5
                   :pos    280})
        :y-axis (viz/linear-axis
                  {:domain      [0 10]
                   :range       [280 20]
                   :major       1
                   :pos         50
                   :label-dist  15
                   :label-style {:text-anchor "end"}})
        :data   [(merge (heatmap-spec id the-matrix) opts)]}
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 300})
       (svg/serialize)
       (spit (str prefix "-" (name id) ".svg"))))

(defn display-state [props grid-cells]
  #_(let [cluster-positions (remove nil? (map (fn [[pos-idx char-vec]]
                                                (let [cluster (::core/cluster char-vec)]
                                                  (if-not (or (= nil cluster)
                                                              (= "NO_CLASS" cluster))
                                                    pos-idx)))
                                              grid-cells))]
      (if (= 2 (::core/dimensions props))
        {"none" cluster-positions}
        (let [_            (println "cluster positions: " cluster-positions)
              input-matrix (matrix/array :vectorz cluster-positions)
              _            (println "input mat;" input-matrix)
              algo-name    :parallel-bht
              _            (println "tsne: " algo-name)
              output-map   {algo-name (tsne/tsne input-matrix 2 :tsne-algorithm algo-name :perplexity 0.01)}]
          output-map)))

  (cartesian-viz "hm" :rainbow2 (grids-2d->heatmap-vec grid-cells props))
  (cartesian-viz "hm" :orange-blue (grids-2d->heatmap-vec grid-cells props))
  )





(deftest stuff
  (let [test-props   {
                      ::core/c_m         3.0
                      ::core/c_l         0.8
                      ::core/lambda      0.998
                      ::core/beta        0.3
                      ::core/dimensions  2
                      ::core/phase-space [
                                          {::core/domain-start    -2.5
                                           ::core/domain-end      2.5
                                           ::core/domain-interval 0.1}
                                          {::core/domain-start    -0.5
                                           ::core/domain-end      0.5
                                           ::core/domain-interval 0.1}]
                      ::core/gap-time    50}
        centroids    (vec (repeatedly 3 (fn [] (get-random-sample :unifrom test-props))))
        _            (println "centroids: " centroids)
        samples      (repeatedly 10000 #(hash-map
                                         ::core/raw-datum
                                         {::core/position-value
                                                       (get-random-sample :normal test-props
                                                                          :centroids
                                                                          centroids)
                                          ::core/value 1.0}))

        test-state   {::core/state {::core/grid-cells           {}
                                    ::core/properties           test-props
                                    ::core/initialized-clusters false}}

        [final-state prof-stats] (profiled {} (core/dstream-iterations test-state samples))
        final-grids  (::core/grid-cells final-state)
        sorted-stats (sort-by #(:mean (second %)) (:id-stats-map prof-stats))
        displayable  (display-state test-props (::core/grid-cells final-state))]

    ;;TODO abandon tsne for a simple dxd grid of 2d scatter plots

    (clojure.pprint/pprint displayable)
    ;(clojure.pprint/pprint sorted-stats)
    )
  )