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


(defn get-random-sample [properties & {:keys [centroids]
                                       }]
  (if centroids
    (let [the-centroid (get centroids (rand-int (count centroids)))]
      (vec
        (map-indexed (fn [idx {:keys [::core/domain-start ::core/domain-end]}]
                       (max domain-start (min domain-end (+ domain-start

                                                            ;(rand (- domain-end domain-start))
                                                            (+ (get the-centroid idx) (first (random/sample-normal 1)))
                                                            ))))
                     (::core/phase-space properties)))  )
    (vec
      (map-indexed (fn [idx {:keys [::core/domain-start ::core/domain-end]}]
                     (max domain-start (min domain-end (+ domain-start

                                                          ;(rand (- domain-end domain-start))
                                                          (first (random/sample-normal 1))
                                                          ))))
                   (::core/phase-space properties)))))


(def test-matrix
  (->> (for [y (range 10) x (range 50)] (n/noise2 (* x 0.1) (* y 0.25)))
       (viz/matrix-2d 50 10)))

(defn heatmap-spec
  [id]
  {:matrix        test-matrix
   :value-domain  (viz/value-domain-bounds test-matrix)
   :palette       (->> id (grad/cosine-schemes) (apply grad/cosine-gradient 100))
   :palette-scale viz/linear-scale
   :layout        viz/svg-heatmap})

(defn cartesian-viz
  [prefix id & [opts]]
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
        :data   [(merge (heatmap-spec id) opts)]}
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 300})
       (svg/serialize)
       (spit (str prefix "-" (name id) ".svg"))))


;;props look slike
;{::N           10000
; ::c_m         3.0
; ::c_l         0.8
; ::lambda      0.998
; ::beta        0.3
; ::dimensions  4
; ::phase-space [
;                {::domain-start    0.0
;                 ::domain-end      1.0
;                 ::domain-interval 0.1}
;                {::domain-start    0.0
;                 ::domain-end      1.0
;                 ::domain-interval 0.1}
;                {::domain-start    0.0
;                 ::domain-end      1.0
;                 ::domain-interval 0.1}
;                {::domain-start    0.0
;                 ::domain-end      1.0
;                 ::domain-interval 0.1}]
; ::gap-time    4}

(def test-grid-cells {[0 1 2 35] {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.11
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "C"
                                  ::core/label                         ::core/sparse}
                      [0 1 2 30] {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.11
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "C"
                                  ::core/label                         ::core/sparse}
                      [0 1 2 3]  {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.11
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "C"
                                  ::core/label                         ::core/sparse}
                      [0 1 2 4]  {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.02
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "C"
                                  ::core/label                         ::core/sparse}
                      [0 1 2 5]  {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.55
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "C"
                                  ::core/label                         ::core/sparse}

                      [0 1 4 5]  {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.55
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "A"
                                  ::core/label                         ::core/sparse}
                      [0 1 5 5]  {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.55
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "A"
                                  ::core/label                         ::core/sparse}

                      [0 1 6 5]  {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.55
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "A"
                                  ::core/label                         ::core/sparse}
                      [10 2 6 5] {::core/last-update-time              0
                                  ::core/last-time-removed-as-sporadic 0
                                  ::core/density-at-last-update        0.55
                                  ::core/sporadicity                   ::core/normal
                                  ::core/cluster                       "B"
                                  ::core/label                         ::core/sparse}})

(defn display-state [grid-cells]

  (let [

        cluster-positions (remove nil? (map (fn [[pos-idx char-vec]]
                                              (let [cluster (::core/cluster char-vec)]
                                                (if-not (or (= nil cluster)
                                                            (= "NO_CLASS" cluster))
                                                  pos-idx)))
                                            grid-cells))
        _                 (println "cluster positions: " cluster-positions)
        input-matrix      (matrix/array :vectorz cluster-positions)
        _                 (println "input mat;" input-matrix)
        output-map        (->> tsne/tsne-algo-names
                               (map (fn [algo-name]
                                      (println algo-name)
                                      [algo-name (tsne/tsne input-matrix 2 :tsne-algorithm algo-name :perplexity 0.01)]))
                               (into {}))]
    output-map
    )

  ;(cartesian-viz "hm" :rainbow2)
  ;(cartesian-viz "hm" :orange-blue)
  )





(deftest stuff
  (let [samples     (repeatedly 2000 #(hash-map
                                       ::core/raw-datum
                                       {::core/position-value
                                                     (get-random-sample {
                                                                         ::core/c_m         3.0
                                                                         ::core/c_l         0.8
                                                                         ::core/lambda      0.998
                                                                         ::core/beta        0.3
                                                                         ::core/dimensions  4
                                                                         ::core/phase-space [
                                                                                             {::core/domain-start    -10.0
                                                                                              ::core/domain-end      10.0
                                                                                              ::core/domain-interval 0.01}
                                                                                             {::core/domain-start    -10.0
                                                                                              ::core/domain-end      10.0
                                                                                              ::core/domain-interval 0.01}
                                                                                             {::core/domain-start    -10.0
                                                                                              ::core/domain-end      10.0
                                                                                              ::core/domain-interval 0.01}
                                                                                             {::core/domain-start    -10.0
                                                                                              ::core/domain-end      10.0
                                                                                              ::core/domain-interval 0.01}]
                                                                         ::core/gap-time    10}
                                                                        :centroids [[0.1 5.0 -5.0 2.0]
                                                                                    [-5.0 -5.0 1.0 3.4]]
                                                                        )
                                        ::core/value 1.0}))

        test-state  {::core/state {::core/grid-cells           {}
                                   ::core/properties           {
                                                                ::core/c_m         3.0
                                                                ::core/c_l         0.8
                                                                ::core/lambda      0.998
                                                                ::core/beta        0.3
                                                                ::core/dimensions  4
                                                                ::core/phase-space [
                                                                                    {::core/domain-start    -10.0
                                                                                     ::core/domain-end      10.0
                                                                                     ::core/domain-interval 0.01}
                                                                                    {::core/domain-start    -10.0
                                                                                     ::core/domain-end      10.0
                                                                                     ::core/domain-interval 0.01}
                                                                                    {::core/domain-start    -10.0
                                                                                     ::core/domain-end      10.0
                                                                                     ::core/domain-interval 0.01}
                                                                                    {::core/domain-start    -10.0
                                                                                     ::core/domain-end      10.0
                                                                                     ::core/domain-interval 0.01}]
                                                                ::core/gap-time    4}
                                   ::core/initialized-clusters false}}


        [final-state prof-stats] (profiled {} (core/dstream-iterations test-state samples))

        final-grids (::core/grid-cells final-state)

        sorted-stats (sort-by #(:mean (second %)) (:id-stats-map prof-stats))
        ]

    ;;TODO abandon tsne for a simple dxd grid of 2d scatter plots

    (clojure.pprint/pprint sorted-stats)
    ;(clojure.pprint/pprint final-grids)
    ;(display-state (::core/grid-cells final-state))
    )
  )