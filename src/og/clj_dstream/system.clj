(ns og.clj-dstream.system
  (:require [og.clj-dstream.server.system :as srv])
  (:gen-class))


(defn start-server [{:keys [port] :or {port 2104}}]
  (srv/start port))

(defn stop-server []
  (srv/stop))

(defn -main
  [& args]
  (println "MAIN: " args)
  (start-server {:port 2104}))