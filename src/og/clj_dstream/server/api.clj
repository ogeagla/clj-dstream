(ns og.clj-dstream.server.api)

(defn put-data [thing]
  (println "Put!" thing)
  thing)

(defn get-clusters [based-on]
  (println "Get!" based-on)
  {:stuff :thing})