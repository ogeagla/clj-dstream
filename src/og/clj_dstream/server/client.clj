(ns og.clj-dstream.server.client
  (:require [og.clj-dstream.server.api :as dapi]
            [slacker.client :refer :all]))

(def conn (slackerc "localhost:2104"))
(defn-remote conn og.clj-dstream.server.api/get-clusters)
(defn-remote conn og.clj-dstream.server.api/put-data)
(defn-remote conn og.clj-dstream.server.api/init-state)
(defn-remote conn og.clj-dstream.server.api/set-props)

(defn run-catch [the-fn]
  (try
    (eval the-fn)
    (catch Exception e
      (println "RPC call failed: " e))))

(defn rpc-set-props [props] (set-props props))
(defn rpc-init-state [] (init-state))
(defn rpc-put-data [samples] (put-data samples))
(defn rpc-get-clusters [a] (get-clusters a))
