(ns og.clj-dstream.server-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.server.system :as srv]
            [og.clj-dstream.server.client :as clt]
            [og.clj-dstream.core :as core]))

(defn my-test-fixture [f]
  (srv/start)
  (f)
  (srv/stop))


(use-fixtures :once my-test-fixture)

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

(deftest test-props-must-be-set
  (do
    (println "set props: " (clt/rpc-set-props props))
    (println "init state: " (clt/rpc-init-state))
    (println "put data: " (clt/rpc-put-data [{::core/raw-datum {::core/position-value [0.2, 0.4] ::core/value 1.0}}]))
    (clt/rpc-get-clusters {:a :b})
    ))