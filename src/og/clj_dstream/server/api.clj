(ns og.clj-dstream.server.api
  (:require [og.clj-dstream.core :as core]))

(def ^{:private true} the-props* (atom {}))
(def ^{:private true} the-state* (atom {}))
(def ^{:private true} props-set* (atom false))

(defn set-props [props]
  (reset! the-props* props)
  (reset! props-set* true)
  @the-props*)

(defn init-state []
  (if @props-set*
    (do (reset! the-state* {::core/grid-cells                 {}
                            ::core/grid-cell-deletion-history {}
                            ::core/properties                 @the-props*
                            ::core/initialized-clusters       false})
        @the-state*)
    (do (println "No props set")
        (throw (RuntimeException. "set-props before init")))))

(defn put-data [samples]

  (println "Put!" samples)

  (reset! the-state* (core/put-data-for-next-time-step @the-state* samples))

  @the-state*)

(defn get-clusters [based-on]
  (println "Get!" based-on)
  {:stuff :thing})