(ns clj-dstream.visualize
  (:require [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.core.vector :as v]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]
            [thi.ng.math.simplexnoise :as n]
            [thi.ng.color.gradients :as grad]
            [think.tsne.core :as tsne]
            [clojure.core.matrix :as matrix]
            [clj-dstream.core :as core]))


(defn heatmap-spec
  [id the-matrix]
  {:matrix        the-matrix
   :value-domain  (viz/value-domain-bounds the-matrix)
   :palette       (->> id (grad/cosine-schemes) (apply grad/cosine-gradient 100))
   :palette-scale viz/linear-scale
   :layout        viz/svg-heatmap})

(defn cartesian-viz
  [prefix id the-matrix rows cols & [opts]]
  (->> {:x-axis (viz/linear-axis
                  {:domain [0 cols]
                   :range  [50 550]
                   :major  10
                   :minor  5
                   :pos    280})
        :y-axis (viz/linear-axis
                  {:domain      [0 rows]
                   :range       [280 20]
                   :major       10
                   :pos         50
                   :label-dist  15
                   :label-style {:text-anchor "end"}})
        :data   [(merge (heatmap-spec id the-matrix) opts)]}
       (viz/svg-plot2d-cartesian)
       (svg/svg {:width 600 :height 300})
       (svg/serialize)
       (spit (str prefix "-" (name id) ".svg"))))

(defn grids-2d->heatmap-vec [grid-cells props & {:keys [clusters-only]}]
  (let [phase-space (::core/phase-space props)
        d1          (first phase-space)
        d2          (second phase-space)
        rows        (int (/ (- (::core/domain-end d1) (::core/domain-start d1)) (::core/domain-interval d1)))
        cols        (int (/ (- (::core/domain-end d2) (::core/domain-start d2)) (::core/domain-interval d2)))]
    (let [vecs
          (for [c (range cols) r (range rows)]
            (let [num (or
                        (when clusters-only
                          (let [cluster (::core/cluster (get grid-cells [r c]))]
                            (if (and (not (= nil cluster))
                                     (not (= "NO_CLASS" cluster)))
                              (do
                                (::core/density-at-last-update (get grid-cells [r c])))
                              (/ (rand) 10000))))
                        (::core/density-at-last-update (get grid-cells [r c]))
                        (/ (rand) 10000))]
              num))]
      {:matrix (->>
                 vecs
                 (viz/matrix-2d rows cols))
       :rows   rows
       :cols   cols})))

(defn display-state [name props grid-cells]
  (if (= 2 (::core/dimensions props))
    (let [hm1 (grids-2d->heatmap-vec grid-cells props)
          hm2 (grids-2d->heatmap-vec grid-cells props :clusters-only true)]

      (cartesian-viz (str "out/grids-" name) :rainbow2 (:matrix hm1) (:cols hm1) (:rows hm1))
      (cartesian-viz (str "out/clusters-" name) :rainbow2 (:matrix hm2) (:cols hm2) (:rows hm2)))
    (let [cluster-positions (remove nil? (map (fn [[pos-idx char-vec]]
                                                (let [cluster (::core/cluster char-vec)]
                                                  (if-not (or (= nil cluster)
                                                              (= "NO_CLASS" cluster))
                                                    pos-idx)))
                                              grid-cells))
          _                 (println "cluster positions: " cluster-positions)
          input-matrix      (matrix/array :vectorz cluster-positions)
          _                 (println "input mat;" input-matrix)
          algo-name         :parallel-bht
          _                 (println "tsne: " algo-name)
          output-map        {algo-name (tsne/tsne input-matrix 2 :tsne-algorithm algo-name :perplexity 0.01)}]
      output-map)))