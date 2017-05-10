(ns og.clj-dstream.normal-sampling-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.core :as core]
            [og.clj-dstream.test-utils :as utils]
            [clojure.core.matrix.random :as random]
            [og.clj-dstream.visualize :as visualize]
            [og.clj-dstream.api :as api]))

(defn time->3-cluster-sample [t time-intervals props]
  (let [percent-complete (/ (float t) (float time-intervals))
        res              (hash-map
                           ::core/raw-datum
                           {::core/position-value
                                         (let [r (rand)]
                                           (cond (< percent-complete 0.6) (utils/sample-circle-2d 0.2 :offsets [0.5 0.5])
                                                 (< percent-complete 0.7) (utils/sample-circle-2d 0.2 :offsets [-0.1 -0.5])
                                                 :else (utils/sample-circle-2d 0.2 :offsets [-0.2 0.5])))
                            ::core/value 1.0})]
    res))
(deftest stuff
  (let [props          {::core/c_m         3.0
                        ::core/c_l         0.8
                        ::core/lambda      0.998
                        ::core/beta        0.3
                        ::core/dimensions  2
                        ::core/phase-space [
                                            {::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.1}
                                            {::core/domain-start    -1.0
                                             ::core/domain-end      1.0
                                             ::core/domain-interval 0.1}]
                        ::core/gap-time    200}
        final-state    (api/iterate-with-sampling-and-visualization! time->3-cluster-sample 10000 "normal-sampling" "normal-sample-out" props 100)
        final-clusters (keys (:clusters-grid-cells (core/state->clusters final-state)))]
    (println "Final clusters: " final-clusters)
    (is (= 1 (count final-clusters)))))