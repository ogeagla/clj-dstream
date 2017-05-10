(ns og.clj-dstream.api
  (:require
    [og.clj-dstream.core :as core]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
    [og.clj-dstream.visualize :as visualize]))

;;TODO

;; w logs
;; w profiling
;; w plotting
;; w persistent state IO

(defn get-clusters [state]
  "Get cilantro"
  (core/state->clusters state))

(defn iterate-without-logging! [init-state data]
  (reset! core/do-logging false)
  (iterate init-state data))

(defn iterate! [init-state data]
  (core/dstream-iterations
    init-state
    data
    :state-append-every
    (int (/ (count data) 50))
    :instrument-spec
    false))

(defn iterate-with-profiling! [init-state data data-per-time-interval]
  (profiled {}
            (core/dstream-iterations
              init-state
              data
              :data-per-time-interval
              data-per-time-interval
              :state-append-every
              (int (/ (count data) 50))
              :instrument-spec
              false)))

(defn iterate-with-sampling-and-visualization!
  ;;TODO make this take a map
  [sampling-fn time-itervals out-name out-dir props data-per-time-interval]
  (let [samples      (map (fn [t]
                            (sampling-fn t time-itervals props))
                          (range time-itervals))
        test-state   {::core/state {::core/grid-cells           {}
                                    ::core/properties           props
                                    ::core/initialized-clusters false}}
        [{:keys [final-state state-ts]} prof-stats] (iterate-with-profiling! test-state samples data-per-time-interval)
        final-grids  (::core/grid-cells final-state)
        sorted-stats (take 5 (sort-by #(* -1 (:mean (second %))) (:id-stats-map prof-stats)))
        displayable  (visualize/display-state out-dir (str out-name "-final-") props (::core/grid-cells final-state))]
    (doall (map-indexed (fn [idx [t staat]]
                          (visualize/display-state out-dir (str out-name "-" (format "%09d" t)) props (::core/grid-cells staat))) state-ts))
    (do (visualize/animate-results (str out-dir "/clusters-*") (str out-dir "/animated-clusters.gif"))
        (visualize/animate-results (str out-dir "/grids-*") (str out-dir "/animated-grids.gif")))
    final-state))