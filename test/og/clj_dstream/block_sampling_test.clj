(ns og.clj-dstream.block-sampling-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.clustering.core :as core]
            [og.clj-dstream.test-utils :as utils]
            [clojure.core.matrix.random :as random]
            [og.clj-dstream.visualize :as visualize]
            [og.clj-dstream.clustering.api :as api]))

(defn time->block-sample [t time-intervals props]
  (let [chooser (rand)
        tuple   (cond
                  (<= 0.0 chooser 0.33) (utils/sample-rect-2d 0.4 0.5 :offsets [0.2 0.5])
                  (<= 0.33 chooser 0.66) (utils/sample-rect-2d 0.4 0.5 :offsets [-0.4 -0.5])
                  (<= 0.66 chooser 1.0) (utils/sample-rect-2d 0.4 0.5 :offsets [0.2 -0.5]))]
    {::core/raw-datum
     {::core/position-value tuple
      ::core/value          1.0}}))

(deftest block-dataset
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
                        ::core/gap-time    5}
        final-state    (api/cluster-sampled-data-experiment
                         {:sampling-fn            time->block-sample
                          :time-intervals         20
                          :out-name               "block-sampling"
                          :out-dir                "block-out"
                          :props                  props
                          :data-per-time-interval 50
                          :disable-logging        true})

        final-clusters (keys (:clusters-grid-cells (core/state->clusters final-state)))]
    (is (<= 3 (count final-clusters)))))