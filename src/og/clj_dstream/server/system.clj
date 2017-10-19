(ns og.clj-dstream.server.system
  (:require [slacker.server :as server]))

(def *servers (atom []))

(defn stop []
  (when (= (count @*servers) 3)
    (do
      (server/stop-slacker-server @*servers)
      (reset! *servers []))))

(defn start []
  (stop)
  (let [[the-tcp-server
         the-http-server
         executors] (server/start-slacker-server
                      [(the-ns 'og.clj-dstream.server.api)]
                      2104)]

    #_(.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable
                               (fn []
                                 (println "About to shutting down slacker server")
                                 (stop)
                                 (println "Server stopped."))))
    (reset! *servers [the-tcp-server the-http-server executors])
    (println "Started server on port 2104")))


