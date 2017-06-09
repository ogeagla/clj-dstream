(ns og.clj-dstream.crescents-sampling-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.core :as core]
            [og.clj-dstream.test-utils :as utils]
            [clojure.core.matrix.random :as random]
            [og.clj-dstream.visualize :as visualize]
            [og.clj-dstream.api :as api]))

(defn sample-crescents-2d [circle-r ring-minor-r ring-major-r]
  (let [random-thing (rand)]
    (if (>= 0.5 random-thing)
      (utils/sample-ring-arc-2d (* -1.0 (/ (Math/PI) 5.0))
                                (+ (Math/PI) (/ (Math/PI) 5.0))
                                ring-minor-r
                                ring-major-r
                                :offsets [(* -1.0 (/ ring-minor-r 2.0))
                                          (/ ring-minor-r 2.0)])
      (utils/sample-ring-arc-2d (- (Math/PI) (/ (Math/PI) 5.0))
                                (+ (* 2.0 (Math/PI)) (/ (Math/PI) 5.0))
                                ring-minor-r
                                ring-major-r
                                :offsets [(/ ring-minor-r 2.0)
                                          (* -1.0 (/ ring-minor-r 2.0))]))))

(defn time->crescents-2d-sample [t time-intervals props]
  (let [phase-space  (::core/phase-space props)
        d1           (first phase-space)
        d2           (second phase-space)
        smallest-dim (min (- (::core/domain-end d1) (::core/domain-start d1))
                          (- (::core/domain-end d2) (::core/domain-start d2)))
        circle-r     (/ smallest-dim 12.0)
        ring-minor-r (+ circle-r (/ smallest-dim 8.0))
        ring-major-r (+ ring-minor-r (/ smallest-dim 13.0))]
    (hash-map
      ::core/raw-datum {::core/position-value
                                     (sample-crescents-2d circle-r ring-minor-r ring-major-r)
                        ::core/value 1.0})))

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
                         {:sampling-fn            time->crescents-2d-sample
                          :time-intervals         50
                          :out-name               "crescents-sampling"
                          :out-dir                "crescents-out"
                          :props                  props
                          :data-per-time-interval 150
                          ;:disable-logging        true
                          })

        final-clusters (keys (:clusters-grid-cells (core/state->clusters final-state)))]
    ;;TODO this actually seems to work as intended
    (is (<= 2 (count final-clusters)))))