(ns og.clj-dstream.server.client
  (:require [og.clj-dstream.server.api :as dapi]
            [environ.core :refer [env]]
            [slacker.client :refer :all]))

(def host (or (env :rpc-client-host) "localhost"))
(def port (or (env :rpc-client-port) "2104"))

(def conn (slackerc (str host ":" port)))
(defn-remote conn og.clj-dstream.server.api/get-clusters)
(defn-remote conn og.clj-dstream.server.api/put-data)
(defn-remote conn og.clj-dstream.server.api/init-state)
(defn-remote conn og.clj-dstream.server.api/set-props)
(defn-remote conn og.clj-dstream.server.api/predict-cluster-or-outlier)

(defn rpc-set-props [props] (set-props props))
(defn rpc-init-state [] (init-state))
(defn rpc-put-data [samples] (put-data samples))
(defn rpc-get-clusters [] (get-clusters))
(defn rpc-predict-cluster-or-outlier [raw-datum] (predict-cluster-or-outlier raw-datum))
