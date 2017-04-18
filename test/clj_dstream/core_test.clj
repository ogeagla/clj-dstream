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
  (let [props    (::core/properties core/test-state)
        char-vec {::core/last-update-time              0
                  ::core/last-time-removed-as-sporadic 0
                  ::core/grid-density-at-last-update   0.11
                  ::core/sporadic-or-normal            ::core/normal
                  ::core/cluster-label                 nil
                  ::core/label                         ::core/dense}
        updated  (core/update-char-vec-label {::core/properties props
                                              ::core/char-vec   char-vec})]
    (is (= ::core/transitional (::core/label updated)))))

(deftest are-neighbors
  (let [pos1 [0 1 2 3 4]
        pos2 [1 1 2 3 4]
        pos3 [1 2 3 3 4]
        pos4 [3 1 2 3 4]]
    (is (= true (core/are-neighbors pos1 pos2)))
    (is (= true (core/are-neighbors pos1 pos2 0)))
    (is (= false (core/are-neighbors pos1 pos2 1)))
    (is (= false (core/are-neighbors pos2 pos3)))
    (is (= false (core/are-neighbors pos3 pos4)))))

(deftest is-grid-group
  (let [candidate-1 [[0 1]
                     [0 2]
                     [0 3]
                     [1 3]
                     [1 4]
                     [2 4]]
        candidate-2 [[0 1]
                     [0 2]
                     [0 3]
                     [1 3]
                     [1 4]
                     [3 4]
                     [4 4]]]
    (is (= true (core/is-grid-group candidate-1)))
    (is (= false (core/is-grid-group candidate-2)))))

(deftest is-inside-or-outside-group
  (let [candidate-1 [1 1]
        candidate-2 [1 2]
        group-1     [[1 1]
                     [1 0]
                     [0 1]]]
    (is (= ::core/inside (core/grid-is-inside-or-outside-group candidate-1 group-1)))
    (is (= ::core/outside (core/grid-is-inside-or-outside-group candidate-2 group-1)))))

(deftest is-grid-cluster
  (let [candidate-1 {[0 0 0] {::core/last-update-time              0
                              ::core/last-time-removed-as-sporadic 0
                              ::core/grid-density-at-last-update   0.11
                              ::core/sporadic-or-normal            ::core/normal
                              ::core/cluster-label                 nil
                              ::core/label                         ::core/dense}}
        candidate-2 {[0 1] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/transitional}
                     [1 1] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/dense}
                     [1 0] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/dense}
                     [1 2] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/dense}
                     [2 1] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/transitional}}

        candidate-3 {[0 1] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/transitional}
                     [1 1] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/sparse}
                     [1 0] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/dense}
                     [1 2] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/dense}
                     [2 1] {::core/last-update-time              0
                            ::core/last-time-removed-as-sporadic 0
                            ::core/grid-density-at-last-update   0.11
                            ::core/sporadic-or-normal            ::core/normal
                            ::core/cluster-label                 nil
                            ::core/label                         ::core/dense}}]
    (is (= true (core/is-grid-cluster candidate-1)))
    (is (= true (core/is-grid-cluster candidate-2)))
    (is (= false (core/is-grid-cluster candidate-3)))))

(deftest initialize-clustering
  (let [state-before {::core/grid-cells           {[0 1 2 3] {::core/last-update-time              0
                                                              ::core/last-time-removed-as-sporadic 0
                                                              ::core/grid-density-at-last-update   0.11
                                                              ::core/sporadic-or-normal            ::core/normal
                                                              ::core/cluster-label                 nil
                                                              ::core/label                         ::core/sparse}
                                                   [0 1 2 4] {::core/last-update-time              0
                                                              ::core/last-time-removed-as-sporadic 0
                                                              ::core/grid-density-at-last-update   0.02
                                                              ::core/sporadic-or-normal            ::core/normal
                                                              ::core/cluster-label                 nil
                                                              ::core/label                         ::core/sparse}
                                                   [0 1 2 5] {::core/last-update-time              0
                                                              ::core/last-time-removed-as-sporadic 0
                                                              ::core/grid-density-at-last-update   0.55
                                                              ::core/sporadic-or-normal            ::core/normal
                                                              ::core/cluster-label                 nil
                                                              ::core/label                         ::core/sparse}}
                      ::core/properties           {::core/N           10000
                                                   ::core/c_m         3.0
                                                   ::core/c_l         0.8
                                                   ::core/lambda      0.998
                                                   ::core/beta        0.3
                                                   ::core/dimensions  4
                                                   ::core/phase-space [
                                                                       {::core/domain-start    0.0
                                                                        ::core/domain-end      1.0
                                                                        ::core/domain-interval 0.1}
                                                                       {::core/domain-start    0.0
                                                                        ::core/domain-end      1.0
                                                                        ::core/domain-interval 0.1}
                                                                       {::core/domain-start    0.0
                                                                        ::core/domain-end      1.0
                                                                        ::core/domain-interval 0.1}
                                                                       {::core/domain-start    0.0
                                                                        ::core/domain-end      1.0
                                                                        ::core/domain-interval 0.1}]
                                                   ::core/gap_time    4}
                      ::core/initialized-clusters true}
        state-after  (core/initial-clustering state-before 1)]
    (is (= 1 (count (filter #(= ::core/dense %) (map ::core/label (vals (::core/grid-cells state-after)))))))
    (is (= 1 (count (filter #(= ::core/transitional %) (map ::core/label (vals (::core/grid-cells state-after)))))))
    (is (= 1 (count (filter #(= ::core/sparse %) (map ::core/label (vals (::core/grid-cells state-after)))))))
    (is (= 1 (count (remove nil? (map ::core/cluster-label (vals (::core/grid-cells state-after)))))))))