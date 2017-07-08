(ns og.clj-dstream.outlier-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.core :as core]
            [og.clj-dstream.test-utils :as utils]
            [clojure.core.matrix.random :as random]
            [og.clj-dstream.visualize :as visualize]
            [og.clj-dstream.api :as api]))

(defn time->block-sample [t time-intervals props]
  {::core/raw-datum
   {::core/position-value (utils/sample-rect-2d 0.4 0.5 :offsets [0.2 -0.5])
    ::core/value          1.0}})

(deftest outliers
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
                        ::core/gap-time    25}
        final-state    (api/cluster-sampled-data-experiment
                         {:sampling-fn            time->block-sample
                          :time-intervals         100
                          :out-name               "outliers-sampling"
                          :out-dir                "outliers-out"
                          :props                  props
                          :data-per-time-interval 50
                          :disable-logging        true
                          :disable-profiling      true
                          :disable-plotting       true})
        final-clusters (keys (:clusters-grid-cells (core/state->clusters final-state)))
        predicted-1    (api/predict-cluster-or-outlier {::core/position-value [0.3 -0.4]
                                                        ::core/value          1.0}
                                                       final-state)
        predicted-2    (api/predict-cluster-or-outlier {::core/position-value [0.0 1.0]
                                                        ::core/value          1.0}
                                                       final-state)]
    ;(is (= 1 (count final-clusters)))
    (is (any? predicted-1))
    (is (= nil predicted-2))))
