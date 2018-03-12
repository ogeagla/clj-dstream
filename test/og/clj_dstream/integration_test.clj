(ns og.clj-dstream.integration-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.server.client :as clt]
            [og.clj-dstream.clustering.core :as core]))

(defn my-test-fixture [f]
  (f)
  )


(use-fixtures :each my-test-fixture)

(def props {::core/c_m         3.0
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
            ::core/gap-time    5})


#_(deftest int-test-props-must-be-set
  (is (= :except
         (try
           (println "\n 2 init state: " (clt/rpc-init-state))
           :shouldnt-reach
           (catch Exception e
             (println "\n-- 2 Caught " e)
             :except))))
  (= props (clt/rpc-set-props props))
  (println "\n 2 init state: " (clt/rpc-init-state))
  (println "\n 2 put data: " (clt/rpc-put-data [{::core/raw-datum
                                                 {::core/position-value
                                                  [0.2, 0.4] ::core/value 1.0}}])))

#_(deftest int-test-put-and-predict-data
  (println "set props: " (clt/rpc-set-props props))
  (println "init state: " (clt/rpc-init-state))
  (let [put-data (map
                   (fn [i]
                     (let [p-data {::core/raw-datum
                                   {::core/position-value [(- 0.8
                                                              (/ i 100000))
                                                           0.4]
                                    ::core/value          1.0}}]
                       (clt/rpc-put-data [p-data])
                       p-data))
                   (range 10000))
        clusters (->>
                   put-data
                   (map
                     (fn [d]
                       (clt/rpc-predict-cluster-or-outlier
                         (::core/raw-datum d))))
                   set)]
    (is (= 1 (count clusters)))
    (is (not= nil (first (vec clusters))))))