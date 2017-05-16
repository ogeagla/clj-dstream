(ns og.clj-dstream.high-dimensional-normal-sampling-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.core :as core]
            [og.clj-dstream.test-utils :as utils]
            [clojure.core.matrix.random :as random]
            [og.clj-dstream.visualize :as visualize]
            [og.clj-dstream.api :as api]))

(defn time->rects [t time-intervals props]
  (let [percent-complete (/ (float t) (float time-intervals))
        res              (hash-map
                           ::core/raw-datum
                           {::core/position-value
                            (let [r (rand)]
                              (cond
                                (<= 0.0 r 0.33) (utils/sample-rect [0.2 0.2 0.2 0.2] [0.2 0.2 0.2 0.2])
                                (<= 0.33 r 0.66) (utils/sample-rect [0.2 0.2 0.2 0.2] [-0.5 -0.5 -0.5 -0.5])
                                (<= 0.66 r 1.0) (utils/sample-rect [0.2 0.2 0.2 0.2] [-0.7 -0.7 0.7 0.7])))

                            ::core/value 1.0})]
    res))
#_(deftest normal-dataset
  (let [props          {::core/c_m         3.0
                        ::core/c_l         0.8
                        ::core/lambda      0.998
                        ::core/beta        0.3
                        ::core/dimensions  4
                        ::core/phase-space [
                                            {::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.05}
                                            {::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.05}
                                            {::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.05}
                                            {::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.05}]
                        ::core/gap-time    3}
        final-state    (api/sample-next-data
                         {:sampling-fn            time->rects
                          :time-intervals         15
                          :out-name               "high-dim-normal-sampling"
                          :out-dir                "high-dim-normal-out"
                          :props                  props
                          :data-per-time-interval 500
                          ;:disable-logging        true
                          })

        final-clusters (keys (:clusters-grid-cells (core/state->clusters final-state)))]
    (is (= 3 (count final-clusters)))
    ))