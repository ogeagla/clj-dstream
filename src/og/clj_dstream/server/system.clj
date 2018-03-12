(ns og.clj-dstream.server.system
  (:require [slacker.server :as server]
            [og.clj-dstream.server.api :as api]))

(def the-server-data* (atom []))

(defn stop []
  (println "** Server Stop **")
  (api/reset)
  (when (= (count @the-server-data*) 3)
    (do
      (server/stop-slacker-server @the-server-data*)
      (reset! the-server-data* []))))

(defn start [port]
  (stop)
  (println "** Server Start **")
  (let [[the-tcp-server
         the-http-server
         executors] (server/start-slacker-server
                      [(the-ns 'og.clj-dstream.server.api)]
                      port)]

    #_(.addShutdownHook (Runtime/getRuntime)
                        (Thread. ^Runnable
                                 (fn []
                                   (println "About to shutting down slacker server")
                                   (stop)
                                   (println "Server stopped."))))
    (reset! the-server-data* [the-tcp-server the-http-server executors])
    (println "Started server on port 2104")))


