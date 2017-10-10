(ns og.clj-dstream.server.client
  (:require [og.clj-dstream.server.api :as dapi]
            [slacker.client :refer :all]))

(defn dostuff []
  (do
    (def sc (slackerc "localhost:2104"))
    (defn-remote sc og.clj-dstream.server.api/get-clusters)
    (defn-remote sc og.clj-dstream.server.api/put-data)
    (get-clusters {:a :b})
    (put-data [{:a :b}])))
