(ns clj-dstream.core-test
  (:require [clojure.test :refer :all]
            [clj-dstream.core :as core]))



(deftest put-test
  (testing "Puts raw data into state"
    (let [test-state    {::core/state core/test-state}
          test-raw-data {::core/raw-datum core/test-raw-data}

          put-state     (core/put (merge test-state test-raw-data {::core/t      2
                                                                   ::core/lambda 0.3}))
          put-state-2   (core/put (merge put-state test-raw-data {::core/t      3
                                                                  ::core/lambda 0.3}))
          ]
      (println "final state: " put-state-2))
    ))

(deftest dstream-iterations
  (testing "Dstream iterations"
    (let [test-state    {::core/state core/test-state}
          test-raw-data (repeat 10 {::core/raw-datum core/test-raw-data})
          final-state   (core/dstream-iterations test-state test-raw-data)])))

(deftest computes-cell-count-from-phase-space
  (let [p-space-1 {::core/phase-space
                   [{::core/domain-start    0.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}]}
        exp-1 10
        p-space-2 {::core/phase-space
                   [{::core/domain-start    -1.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}]}
        exp-2 20
        p-space-3 {::core/phase-space
                   [{::core/domain-start    -1.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}
                    {::core/domain-start    -1.0
                     ::core/domain-end      1.0
                     ::core/domain-interval 0.1}]}
        exp-3 400]
    (is (= exp-1 (core/phase-space->cell-count p-space-1)))
    (is (= exp-2 (core/phase-space->cell-count p-space-2)))
    (is (= exp-3 (core/phase-space->cell-count p-space-3)))))