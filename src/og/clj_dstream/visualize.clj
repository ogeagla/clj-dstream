(ns og.clj-dstream.visualize
  (:require [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]
            ;[thi.ng.math.simplexnoise :as n]
            [thi.ng.color.gradients :as grad]
            [think.tsne.core :as tsne]
            [clojure.core.matrix :as matrix]
            [og.clj-dstream.core :as core]
            [me.raynes.fs :as fs]
            [clojure.java.shell :refer [sh]]))

;;TODO make tsne plots have different colors for diff clusters

(defn export-scatter-viz
  [data spec path]
  (->>
    data
    spec
    (viz/svg-plot2d-cartesian)
    (svg/svg {:width 600 :height 600})
    (svg/serialize)
    (spit path)))

(defn scatter-spec [data]
  {:x-axis (viz/linear-axis
             {:domain      [-100.0 100.0]
              :range       [550 20]
              :major       10
              :minor       5
              :pos         50
              :label-dist  15
              :label-style {:text-anchor "end"}})
   :y-axis (viz/linear-axis
             {:domain      [-100.0 100.0]
              :range       [550 20]
              :major       10
              :minor       5
              :pos         50
              :label-dist  15
              :label-style {:text-anchor "end"}})
   :grid   {:attribs {:stroke "#caa"}
            :minor-x true
            :minor-y true}
   :data   [
            {:values  data
             :attribs {:fill "#0af" :stroke "none"}
             :layout  viz/svg-scatter-plot}
            ;{:values  (map (juxt identity #(m/random %)) (range 0 200 2))
            ; :attribs {:fill "none" :stroke "#f60"}
            ; :shape   (viz/svg-triangle-down 6)
            ; :layout  viz/svg-scatter-plot}
            ]
   })

;(export-viz spec "scatter-linear.svg")
;
;(-> spec
;    (export-viz "scatter-log.svg"))
;
;
;
;

(defn cartesian-viz-scatter
  [the-matrix]
  ())

(defn scatter-spec
  [x-size y-size the-matrix & [opts]]
  (->> {:x-axis (viz/linear-axis
                  {:domain [0 x-size]
                   :range [50 550]
                   :major 50
                   :minor 25
                   :pos 580})
        :y-axis (viz/linear-axis
                  {:domain [0 y-size]
                   :range [580 280]
                   :minor 50
                   :pos 50})
        :data [(merge (cartesian-viz-scatter the-matrix) opts)]}))

(defn heatmap-spec
  [id the-matrix]
  {:matrix        the-matrix
   :value-domain  (viz/value-domain-bounds the-matrix)
   :palette       (->> id (grad/cosine-schemes) (apply grad/cosine-gradient 100))
   :palette-scale viz/linear-scale
   :layout        viz/svg-heatmap})

(defn cartesian-viz-heatmap
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
       (svg/svg {:width 600 :height 600})
       (svg/serialize)
       (spit (str prefix "-" (name id) ".svg"))))

(defn grids-2d->heatmap-vec [grid-cells props & {:keys [clusters-only]}]
  (let [phase-space     (::core/phase-space props)
        d1              (first phase-space)
        d2              (second phase-space)
        rows            (int (/ (- (::core/domain-end d1) (::core/domain-start d1)) (::core/domain-interval d1)))
        cols            (int (/ (- (::core/domain-end d2) (::core/domain-start d2)) (::core/domain-interval d2)))
        clusters        (distinct
                          (remove #(not (core/is-cluster? %))
                                  (map ::core/cluster (vals grid-cells))))
        clusters-colors (into {}
                              (map (fn [c]
                                     [c (rand 100.0)]
                                     ) clusters))]
    (let [vecs
          (for [c (range cols) r (range rows)]
            (let [num (or
                        (when clusters-only
                          (let [cluster (::core/cluster (get grid-cells [r c]))]
                            (if (core/is-cluster? cluster)
                              (do
                                (get clusters-colors cluster))
                              (/ (rand) 1000))))
                        (::core/density-at-last-update (get grid-cells [r c]))
                        (/ (rand) 1000))]
              num))]
      {:matrix (->>
                 vecs
                 (viz/matrix-2d rows cols))
       :rows   rows
       :cols   cols})))

(defn animate-results [glob output]
  (println "Creating animated gif from files, to file: " glob " -> " output)
  (try
    (sh "convert" "-delay" "25" "-loop" "0" glob output)
    (catch Throwable t
      (println "Could not create animated gif:" t))))

(defn get-cluster-position-values [grid-cells props]
  (into {} (remove nil? (map-indexed (fn [idx [pos-idx char-vec]]
                                       (let [cluster (::core/cluster char-vec)]
                                         (if (core/is-cluster? cluster)
                                           [(core/position-index->position-value pos-idx (::core/phase-space props))
                                            {:cluster cluster
                                             :idx     idx}])))
                                     grid-cells))))

(defn display-state [dir name props grid-cells]
  (if-not (fs/exists? dir)
    (fs/mkdir dir))
  (if (= 2 (::core/dimensions props))
    (let [hm1 (grids-2d->heatmap-vec grid-cells props)
          hm2 (grids-2d->heatmap-vec grid-cells props :clusters-only true)]

      (cartesian-viz-heatmap (str dir "/grids-" name) :orange-blue (:matrix hm1) (:cols hm1) (:rows hm1))
      (cartesian-viz-heatmap (str dir "/clusters-" name) :orange-blue (:matrix hm2) (:cols hm2) (:rows hm2)))
    (do
      ;      (throw (ex-info "Unsupported dimensionality" props))
      (let [cluster-positions (get-cluster-position-values grid-cells props)
            _                 (println "cluster poss: " (count cluster-positions))
            algo-name         :parallel-bht]
        (when (< 0 (count cluster-positions))
          (let [tsne-output (tsne/tsne
                              (matrix/array :vectorz (keys cluster-positions))
                              2
                              :tsne-algorithm algo-name
                              ;:perplexity 0.01
                              )]
            (export-scatter-viz tsne-output scatter-spec (str dir "/clusters-" name ".svg"))))))))