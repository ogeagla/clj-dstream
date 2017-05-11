(ns og.clj-dstream.crater-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.api :as api]
            [og.clj-dstream.core :as core]
            [og.clj-dstream.test-utils :as utils]))

(defn sample-crater-2d [circle-r ring-minor-r ring-major-r]
  (let [random-thing (rand)]
    (if (>= 0.07 random-thing)
      (utils/sample-circle-2d circle-r)
      (utils/sample-ring-2d ring-minor-r ring-major-r))))

(defn time->crater-2d-sample [t time-intervals props]
  (let [phase-space  (::core/phase-space props)
        d1           (first phase-space)
        d2           (second phase-space)
        smallest-dim (min (- (::core/domain-end d1) (::core/domain-start d1))
                          (- (::core/domain-end d2) (::core/domain-start d2)))
        circle-r     (/ smallest-dim 12.0)
        ring-minor-r (+ circle-r (/ smallest-dim 4.0))
        ring-major-r (+ ring-minor-r (/ smallest-dim 12.0))]
    (hash-map
      ::core/raw-datum {::core/position-value
                                     (sample-crater-2d circle-r ring-minor-r ring-major-r)
                        ::core/value 1.0})))

(deftest crater-dataset
  (let [props {::core/c_m         3.0
               ::core/c_l         0.8
               ::core/lambda      0.998
               ::core/beta        0.3
               ::core/dimensions  2
               ::core/phase-space [{::core/domain-start    -1.0
                                    ::core/domain-end      1.0
                                    ::core/domain-interval 0.1}
                                   {::core/domain-start    -1.0
                                    ::core/domain-end      1.0
                                    ::core/domain-interval 0.1}]
               ::core/gap-time    5}]

    (api/sample-next-data
      {:sampling-fn            time->crater-2d-sample
       :time-intervals         30
       :out-name               "crater-sampling"
       :out-dir                "crater-out"
       :props                  props
       :data-per-time-interval 1000})))