(ns og.clj-dstream.server.system
  (:require [slacker.server :as server]))

(def *servers (atom []))

(defn run []
  (let [[the-tcp-server
         the-http-server
         executors] (server/start-slacker-server
                      [(the-ns 'og.clj-dstream.server.api)]
                      2104)]
    (reset! *servers [the-tcp-server the-http-server executors])))

(defn stop []
  (when (= (count @*servers) 3)
    (do
      (server/stop-slacker-server @*servers)
      (reset! *servers []))))
