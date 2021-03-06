(ns og.clj-dstream.clustering.api
  (:require
    [og.clj-dstream.clustering.core :as core]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
    [og.clj-dstream.visualize :as visualize]))

(defn get-clusters [state]
  "Get clusters"
  (core/state->clusters state))

(defn predict-cluster-or-outlier [raw-datum state]
  "For a state (trained) and a raw datum,
  get the cluster the data would belong to or nil,
  where nil means the datum is an outlier of the model"
  (let [idx        (core/position-value->position-index
                     (merge raw-datum (::core/properties state)))
        grid-cells (::core/grid-cells state)]
    (when (contains? grid-cells idx)
      (::core/cluster (get grid-cells idx)))))

(defn put-next-data [state data]
  "Take a seq of raw data representing the next time step
   and updates the state accordingly; returns updated state"
  (core/put-data-for-next-time-step state data))

(defn- iterate-samples [parted-samples the-state* plot-every-nth state-appender*]
  (doseq [samps parted-samples]
    (reset! the-state*
            (put-next-data @the-state* samps))
    (when (= 0
             (mod
               (::core/current-time @the-state*)
               plot-every-nth))
      (do
        (let [clusters (remove
                         #(not (core/is-cluster? %))
                         (distinct
                           (map ::core/cluster
                                (map
                                  second
                                  (::core/grid-cells @the-state*)))))]
          (core/log-it (::core/current-time @the-state*)
                       ::log-state-periodically
                       {:cluster-count
                                               (count
                                                 clusters)
                        :deletion-history-size (count
                                                 (::core/grid-cell-deletion-history
                                                   @the-state*))
                        :top-3-cluster-sizes   (into
                                                 {}
                                                 (->>
                                                   clusters
                                                   (map (fn [c]
                                                          [c
                                                           (core/cluster->size
                                                             c
                                                             @the-state*)]))
                                                   (sort-by second)
                                                   (take 3)))
                        :grid-count            (count (::core/grid-cells @the-state*))
                        :N                     (::core/N (::core/properties @the-state*))}))
        (swap! state-appender*
               assoc
               (::core/current-time @the-state*)
               @the-state*)))))

(defn cluster-sampled-data-experiment
  [{:keys [sampling-fn
           props
           time-intervals
           data-per-time-interval
           out-name
           out-dir
           disable-logging
           disable-profiling
           disable-plotting]}]
  "Given a sampling function and some information about
  how much data to put per time step and total time steps,
  run the clustering algorithm and plot the results in a nice
  way to a directory"
  (when disable-logging (reset! core/do-logging false))
  (let [total-data      (* time-intervals data-per-time-interval)
        samples         (map (fn [t]
                               (sampling-fn t total-data props))
                             (range total-data))
        the-state*      (atom {::core/grid-cells                 {}
                               ::core/grid-cell-deletion-history {}
                               ::core/properties                 props
                               ::core/initialized-clusters       false})
        parted-samples  (partition data-per-time-interval samples)

        plot-every-nth  (max 1 (int (/ time-intervals 50.0)))
        state-appender* (atom {})

        _               (if disable-profiling
                          (iterate-samples
                            parted-samples
                            the-state*
                            plot-every-nth
                            state-appender*)
                          (let [[_ prof-stats] (profiled {}
                                                         (iterate-samples
                                                           parted-samples
                                                           the-state*
                                                           plot-every-nth
                                                           state-appender*))
                                sorted-stats (take 5
                                                   (sort-by #(* -1 (:mean (second %)))
                                                            (:id-stats-map prof-stats)))]
                            (println "Stats: ")
                            (clojure.pprint/pprint sorted-stats)))

        final-state     @the-state*
        final-grids     (::core/grid-cells final-state)]

    (if-not disable-plotting (do
                               (visualize/display-state
                                 out-dir
                                 (str out-name "-final")
                                 props
                                 (::core/grid-cells final-state))
                               (doall (map-indexed (fn [idx [t staat]]
                                                     (visualize/display-state
                                                       out-dir
                                                       (str out-name "-" (format "%09d" t))
                                                       props
                                                       (::core/grid-cells staat)))
                                                   @state-appender*))
                               (visualize/animate-results
                                 (str out-dir "/clusters-*")
                                 (str out-dir "/animated-clusters.gif"))
                               (visualize/animate-results
                                 (str out-dir "/grids-*")
                                 (str out-dir "/animated-grids.gif"))))
    final-state))
