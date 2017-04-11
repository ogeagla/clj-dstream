(ns clj-dstream.core-test
  (:require [clojure.test :refer :all]
            [clj-dstream.core :as core]))

(def test-state
  {::grid-cells           {[0 1 2 3] {::last-update-time              0
                                      ::last-time-removed-as-sporadic 0
                                      ::grid-density-at-last-update   0.11
                                      ::sporadic-or-normal            ::normal
                                      ::cluster-label                 nil
                                      ::label                         ::dense}}
   ::properties           {::N           10000
                           ::c_m         3.0
                           ::c_l         0.8
                           ::lambda      0.998
                           ::beta        0.3
                           ::dimensions  4
                           ::phase-space [
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}]
                           ::gap_time    4}
   ::initialized-clusters true})

(def test-raw-data
  {::value          0.1
   ::position-value [0.5 0.5 0.5 0.5]})


(deftest put-test
  (testing "Puts raw data into state"
    (let [test-state    {::core/state test-state}
          test-raw-data {::core/raw-datum test-raw-data}

          put-state     (core/put (merge test-state test-raw-data {::core/t      2
                                                                   ::core/lambda 0.3}))
          put-state-2   (core/put (merge put-state test-raw-data {::core/t      3
                                                                  ::core/lambda 0.3}))
          ]
      (println "final state: " put-state-2))
    ))

(deftest dstream-iterations
  (testing "Dstream iterations"
    (let [test-state    {::core/state test-state}
          test-raw-data (repeat 10 {::core/raw-datum test-raw-data})
          final-state   (core/dstream-iterations test-state test-raw-data)])))

(deftest computes-cell-count-from-phase-space
  (let [p-space-1 {::core/phase-space
                   [{::core/domain-start    0.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}]}
        exp-1     10
        p-space-2 {::core/phase-space
                   [{::core/domain-start    -1.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}]}
        exp-2     20
        p-space-3 {::core/phase-space
                   [{::core/domain-start    -1.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}
                    {::core/domain-start    -1.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}]}
        exp-3     400]
    (is (= exp-1 (core/phase-space->cell-count p-space-1)))
    (is (= exp-2 (core/phase-space->cell-count p-space-2)))
    (is (= exp-3 (core/phase-space->cell-count p-space-3)))))

(deftest updates-char-vec-label
  (let [props    (::core/properties test-state)
        char-vec {::core/last-update-time              0
                  ::core/last-time-removed-as-sporadic 0
                  ::core/grid-density-at-last-update   0.11
                  ::core/sporadic-or-normal            ::core/normal
                  ::core/cluster-label                 nil
                  ::core/label                         ::core/dense}
        updated  (core/update-char-vec-label {::core/properties props
                                              ::core/char-vec   char-vec})]
    (is (= ::core/transitional (::core/label updated)))))