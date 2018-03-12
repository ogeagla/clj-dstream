(ns og.clj-dstream.system
  (:require [og.clj-dstream.server.system :as srv]
            [environ.core :refer [env]])
  (:gen-class))


(defn start-server [{:keys [port] :or {port 2104}}]
  (srv/start port))

(defn stop-server []
  (srv/stop))

(defn -main
  [& args]
  (let [rpc-port (env :rpc-port)]
    (println "MAIN- rpc-port:" rpc-port)
    (start-server rpc-port)))