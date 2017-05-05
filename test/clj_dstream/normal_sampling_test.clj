(ns clj-dstream.normal-sampling-test
  (:require [clojure.test :refer :all]
            [clj-dstream.core :as core]
            [clj-dstream.test-utils :as test-utils]
            [clojure.core.matrix.random :as random]
            [clj-dstream.visualize :as visualize]
            [clj-dstream.api :as api]))

(defn sample-at-time [t time-intervals props]
  (hash-map
    ::core/raw-datum
    {::core/position-value
                  (vec (take (::core/dimensions props)
                             (repeatedly
                               (fn []
                                 (let [r (rand)]
                                   (cond (< t (int (/ time-intervals 3))) (+ 0.05
                                                                             (* 0.2
                                                                                (first
                                                                                  (random/sample-normal 1))))
                                         (< t (int (* 2 (/ time-intervals 3)))) (-
                                                                                  (* 0.2
                                                                                     (first
                                                                                       (random/sample-normal 1)))
                                                                                  0.4)
                                         :else (+ 0.3
                                                  (* 0.2
                                                     (first
                                                       (random/sample-normal 1))))))))))
     ::core/value 1.0}))

(deftest stuff
  (let [props {::core/c_m         3.0
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
               ::core/gap-time    200}]
    (api/iterate-with-sampling sample-at-time 1000 "normal-sampling" props)))