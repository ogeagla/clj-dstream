(ns og.clj-dstream.core-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.core :as core]
            [og.clj-dstream.test-utils :as test-utils]))

(def test-state
  {::core/current-time               0
   ::core/data-count                 0
   ::core/grid-cell-deletion-history {}
   ::core/grid-cells                 {[10 1 2 2] {::core/last-update-time        0
                                                  ::core/density-at-last-update  0.13
                                                  ::core/sporadicity             ::core/normal
                                                  ::core/cluster                 nil
                                                  ::core/label                   ::core/sparse
                                                  ::core/last-time-label-changed 0}
                                      [10 1 2 3] {::core/last-update-time        0
                                                  ::core/density-at-last-update  0.3
                                                  ::core/sporadicity             ::core/normal
                                                  ::core/cluster                 nil
                                                  ::core/label                   ::core/sparse
                                                  ::core/last-time-label-changed 0}
                                      [0 1 2 3]  {::core/last-update-time        0
                                                  ::core/density-at-last-update  0.11
                                                  ::core/sporadicity             ::core/normal
                                                  ::core/cluster                 nil
                                                  ::core/label                   ::core/sparse
                                                  ::core/last-time-label-changed 0}
                                      [0 1 2 4]  {::core/last-update-time        0
                                                  ::core/density-at-last-update  0.02
                                                  ::core/sporadicity             ::core/normal
                                                  ::core/cluster                 nil
                                                  ::core/label                   ::core/sparse
                                                  ::core/last-time-label-changed 0}
                                      [0 1 2 5]  {::core/last-update-time        0
                                                  ::core/density-at-last-update  0.55
                                                  ::core/sporadicity             ::core/normal
                                                  ::core/cluster                 nil
                                                  ::core/label                   ::core/sparse
                                                  ::core/last-time-label-changed 0}
                                      [0 1 3 5]  {::core/last-update-time        0
                                                  ::core/density-at-last-update  0.66
                                                  ::core/sporadicity             ::core/normal
                                                  ::core/cluster                 nil
                                                  ::core/label                   ::core/sparse
                                                  ::core/last-time-label-changed 0}}
   ::core/properties                 {::core/N                10000
                                      ::core/c_m              3.0
                                      ::core/c_l              0.8
                                      ::core/lambda           0.998
                                      ::core/beta             0.3
                                      ::core/dimensions       4
                                      ::core/last-update-time 0
                                      ::core/phase-space      [
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
                                      ::core/gap-time         4}
   ::core/initialized-clusters       true})

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
        char-vec {::core/last-time-label-changed       0
                  ::core/last-update-time              0
                  ::core/density-at-last-update        0.11
                  ::core/sporadicity                   ::core/normal
                  ::core/cluster                       nil
                  ::core/label                         ::core/dense}
        updated  (core/update-char-vec-label {::core/properties   props
                                              ::core/char-vec     char-vec
                                              ::core/current-time 1})]
    (is (= ::core/transitional (::core/label updated)))))

(deftest are-neighbors
  (let [pos1 [0 1 2 3 4]
        pos2 [1 1 2 3 4]
        pos3 [1 2 3 3 4]
        pos4 [3 1 2 3 4]]
    (is (= true (core/are-neighbors {:pos-idxs-1 pos1 :pos-idxs-2 pos2})))
    (is (= true (core/are-neighbors {:pos-idxs-1 pos1 :pos-idxs-2 pos2 :neigh-dim 0})))
    (is (= false (core/are-neighbors {:pos-idxs-1 pos1 :pos-idxs-2 pos2 :neigh-dim 1})))
    (is (= false (core/are-neighbors {:pos-idxs-1 pos2 :pos-idxs-2 pos3})))
    (is (= false (core/are-neighbors {:pos-idxs-1 pos3 :pos-idxs-2 pos4})))))

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
    (is (= ::core/inside (core/pos-is-inside-or-outside-group candidate-1 group-1)))
    (is (= ::core/outside (core/pos-is-inside-or-outside-group candidate-2 group-1)))))

(deftest is-grid-cluster
  (let [candidate-1 {[0 0 0] {::core/last-update-time              0
                              ::core/density-at-last-update        0.11
                              ::core/sporadicity                   ::core/normal
                              ::core/cluster                       nil
                              ::core/label                         ::core/dense
                              ::core/last-time-label-changed       0}}
        candidate-2 {[0 1] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/transitional
                            ::core/last-time-label-changed       0}
                     [1 1] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/dense
                            ::core/last-time-label-changed       0}
                     [1 0] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/dense
                            ::core/last-time-label-changed       0}
                     [1 2] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/dense
                            ::core/last-time-label-changed       0}
                     [2 1] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/transitional
                            ::core/last-time-label-changed       0}}

        candidate-3 {[0 1] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/transitional
                            ::core/last-time-label-changed       0}
                     [1 1] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/sparse
                            ::core/last-time-label-changed       0}
                     [1 0] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/dense
                            ::core/last-time-label-changed       0}
                     [1 2] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/dense
                            ::core/last-time-label-changed       0}
                     [2 1] {::core/last-update-time              0
                            ::core/density-at-last-update        0.11
                            ::core/sporadicity                   ::core/normal
                            ::core/cluster                       nil
                            ::core/label                         ::core/dense
                            ::core/last-time-label-changed       0}}]
    (is (= true (core/is-grid-cluster candidate-1)))
    (is (= true (core/is-grid-cluster candidate-2)))
    (is (= false (core/is-grid-cluster candidate-3)))))

(deftest initialize-clustering
  (let [state-before-1 {::core/current-time         0
                        ::core/data-count           0
                        ::core/grid-cell-deletion-history {}
                        ::core/grid-cells           {[0 1 2 3] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.11
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}
                                                     [0 1 2 4] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.02
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}
                                                     [0 1 2 5] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.55
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}}
                        ::core/properties           {::core/N           10000
                                                     ::core/c_m         3.0
                                                     ::core/c_l         0.8
                                                     ::core/lambda      0.998
                                                     ::core/beta        0.3
                                                     ::core/dimensions  4
                                                     ::core/phase-space [{::core/domain-start    0.0
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
                                                     ::core/gap-time    4}
                        ::core/initialized-clusters true}
        state-before-2 {::core/current-time         0
                        ::core/data-count           0
                        ::core/grid-cell-deletion-history {}
                        ::core/grid-cells           {[0 1 2 3] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.11
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}
                                                     [0 1 2 4] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.02
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}
                                                     [0 1 2 5] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.55
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}

                                                     [0 1 4 5] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.55
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}
                                                     [0 1 5 5] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.55
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}

                                                     [0 1 6 5] {::core/last-update-time              0
                                                                ::core/density-at-last-update        0.55
                                                                ::core/sporadicity                   ::core/normal
                                                                ::core/cluster                       nil
                                                                ::core/label                         ::core/sparse
                                                                ::core/last-time-label-changed       0}}
                        ::core/properties           {::core/N           10000
                                                     ::core/c_m         3.0
                                                     ::core/c_l         0.8
                                                     ::core/lambda      0.998
                                                     ::core/beta        0.3
                                                     ::core/dimensions  4
                                                     ::core/phase-space [{::core/domain-start    0.0
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
                                                     ::core/gap-time    4}
                        ::core/initialized-clusters true}
        state-after-1  (core/initial-clustering (atom state-before-1) 1)
        state-after-2  (core/initial-clustering (atom state-before-2) 1)]
    (is (= 1 (test-utils/label-count state-after-1 ::core/dense)))
    (is (= 1 (test-utils/label-count state-after-1 ::core/transitional)))
    (is (= 1 (test-utils/label-count state-after-1 ::core/sparse)))
    (is (= 1 (test-utils/cluster-count state-after-1)))
    (is (= 2 (test-utils/cluster-count state-after-2)))))

(deftest split-cluster
  (let [cluster-1 {[0 1 2 3]  {::core/last-update-time              0
                               ::core/density-at-last-update        0.11
                               ::core/sporadicity                   ::core/normal
                               ::core/cluster                       "A"
                               ::core/label                         ::core/sparse}
                   [0 1 2 4]  {::core/last-update-time              0
                               ::core/density-at-last-update        0.02
                               ::core/sporadicity                   ::core/normal
                               ::core/cluster                       "A"
                               ::core/label                         ::core/sparse}
                   [0 1 2 5]  {::core/last-update-time              0
                               ::core/density-at-last-update        0.55
                               ::core/sporadicity                   ::core/normal
                               ::core/cluster                       "A"
                               ::core/label                         ::core/sparse}

                   [0 1 4 5]  {::core/last-update-time              0
                               ::core/density-at-last-update        0.55
                               ::core/sporadicity                   ::core/normal
                               ::core/cluster                       "A"
                               ::core/label                         ::core/sparse}
                   [0 1 5 5]  {::core/last-update-time              0
                               ::core/density-at-last-update        0.55
                               ::core/sporadicity                   ::core/normal
                               ::core/cluster                       "A"
                               ::core/label                         ::core/sparse}

                   [0 1 6 5]  {::core/last-update-time              0
                               ::core/density-at-last-update        0.55
                               ::core/sporadicity                   ::core/normal
                               ::core/cluster                       "A"
                               ::core/label                         ::core/sparse}
                   [10 2 6 5] {::core/last-update-time              0
                               ::core/density-at-last-update        0.55
                               ::core/sporadicity                   ::core/normal
                               ::core/cluster                       "A"
                               ::core/label                         ::core/sparse}}
        split-1   (core/split-cluster cluster-1)]
    (is (= 3 (count split-1)))
    (is (= (set (keys cluster-1))
           (set (mapcat identity (map keys split-1)))))))
