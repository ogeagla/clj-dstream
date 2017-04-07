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
          final-state   (core/dstream-iterations test-state test-raw-data)]
      (clojure.pprint/pprint final-state))))