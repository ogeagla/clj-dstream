(ns og.clj-dstream.server-test
  (:require [clojure.test :refer :all]
            [og.clj-dstream.server.system :as srv]
            [og.clj-dstream.server.client :as clt]))

(defn my-test-fixture [f]
  (srv/run)
  (f)
  (srv/stop))


(use-fixtures :once my-test-fixture)

(deftest test-using-server
  (clt/dostuff))