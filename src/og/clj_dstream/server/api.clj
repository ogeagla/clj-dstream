(ns og.clj-dstream.server.api
  (:require [og.clj-dstream.core :as core]))

(def ^{:private true} the-props* (atom {}))
(def ^{:private true} the-state* (atom {}))
(def ^{:private true} props-set* (atom false))
(def ^{:private true} api-settings (atom {
                                          ::data-per-time-interval 10
                                          ::analyze-nth-timestep 5
                                          ::enable-logging true
                                          ::enable-profiling true
                                          ::enable-plotting true
                                          }))

(defn reset []
  (reset! the-props* {})
  (reset! the-state* {})
  (reset! props-set* false))

(defn set-props [props]
  (println "\n API - set props")
  (reset! the-props* props)
  (reset! props-set* true)
  @the-props*)

(defn init-state []
  (if @props-set*
    (do
      (println "\nAPI - init state")
      (reset! the-state* {::core/grid-cells                 {}
                            ::core/grid-cell-deletion-history {}
                            ::core/properties                 @the-props*
                            ::core/initialized-clusters       false})
        @the-state*)
    (do (println "\nAPI - No props set")
        (throw (RuntimeException. "API - set-props before init")))))

(defn put-data [samples]

  (println "\n API - Put!" samples)

  (reset! the-state* (core/put-data-for-next-time-step @the-state* samples))

  @the-state*)

(defn get-clusters [based-on]
  (println "\n API - Get!" based-on)
  {:stuff :thing})