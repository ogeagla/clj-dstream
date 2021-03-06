(ns og.clj-dstream.time-dependent-normal-sampling-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.clustering.core :as core]
            [og.clj-dstream.test-utils :as utils]
            [clojure.core.matrix.random :as random]
            [og.clj-dstream.visualize :as visualize]
            [og.clj-dstream.clustering.api :as api]))

(defn time->3-cluster-sample [t time-intervals props]
  (let [percent-complete (/ (float t) (float time-intervals))
        res
                         (hash-map
                           ::core/raw-datum
                           {::core/position-value
                                         (let [size 0.3]
                                           (cond
                                             (< percent-complete 0.1) (utils/sample-circle-2d size :offsets [0.2 0.2])
                                             (< percent-complete 0.15) (utils/sample-circle-2d size :offsets [0.15 0.15])
                                             (< percent-complete 0.2) (utils/sample-circle-2d size :offsets [0.1 0.1])
                                             (< percent-complete 0.25) (utils/sample-circle-2d size :offsets [0.0 0.0])
                                             (< percent-complete 0.3) (utils/sample-circle-2d size :offsets [-0.05 -0.05])
                                             (< percent-complete 0.35) (utils/sample-circle-2d size :offsets [-0.1 -0.1])
                                             (< percent-complete 0.4) (utils/sample-circle-2d size :offsets [-0.15 -0.15])
                                             (< percent-complete 0.5) (utils/sample-circle-2d size :offsets [-0.2 -0.2])
                                             (< percent-complete 0.55) (utils/sample-circle-2d size :offsets [-0.2 -0.2])
                                             (< percent-complete 0.6) (utils/sample-circle-2d size :offsets [-0.25 -0.1])
                                             (< percent-complete 0.65) (utils/sample-circle-2d size :offsets [-0.3 0.0])
                                             (< percent-complete 0.7) (utils/sample-circle-2d size :offsets [-0.3 0.1])
                                             (< percent-complete 0.75) (utils/sample-circle-2d size :offsets [-0.3 0.15])
                                             (< percent-complete 0.8) (utils/sample-circle-2d size :offsets [-0.3 0.2])
                                             (< percent-complete 0.85) (utils/sample-circle-2d size :offsets [-0.3 0.25])
                                             :else (utils/sample-circle-2d size :offsets [-0.3 0.3])))
                            ::core/value 1.0})]
    res))
(deftest normal-dataset
  (let [props          {::core/c_m         3.0
                        ::core/c_l         0.8
                        ::core/lambda      0.998
                        ::core/beta        0.3
                        ::core/dimensions  2
                        ::core/phase-space [{::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.05}
                                            {::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.1}]
                        ::core/gap-time    4}
        final-state    (api/cluster-sampled-data-experiment
                         {:sampling-fn            time->3-cluster-sample
                          :time-intervals         500
                          :out-name               "moving-normal-sampling"
                          :out-dir                "moving-normal-out"
                          :props                  props
                          :data-per-time-interval 50
                          :disable-logging        false
                          :disable-profiling      false
                          :disable-plotting       false})

        final-clusters (keys (:clusters-grid-cells (core/state->clusters final-state)))]
    (is (= 1 (count final-clusters)))))
