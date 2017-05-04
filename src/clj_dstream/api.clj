(ns clj-dstream.api
  (:require
    [clj-dstream.core :as core]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;;TODO

;; w logs
;; w profiling
;; w plotting
;; w persistent state IO

(defn run-with-profiling []
  (let [[{:keys [final-state state-ts]} prof-stats] (profiled {} (core/dstream-iterations test-state samples :state-append-every (int (/ time-intervals 20))))])
  )

