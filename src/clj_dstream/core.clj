(ns clj-dstream.core
  (:gen-class)
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))




(s/def ::label #{::dense ::sparse ::transitional})

(s/def ::status #{::sporadic ::normal})

(s/def ::value float?)

(s/def ::time int?)

(s/def ::dimensions pos-int?)

(s/def ::position (s/and vector? #(= ::dimensions (count %))))

(s/def ::domain-start float?)
(s/def ::domain-end float?)
(s/def ::domain-interval (s/and float? pos?))
(s/def ::domain (s/keys :req [::domain-start ::domain-end ::domain-interval]))
(s/def ::domains (s/coll-of ::domain))

(s/def ::time-point (s/keys :req [::t
                                  ::value
                                  ::status
                                  ::label]
                            :opt [::cluster]))

(s/def ::time-points (s/coll-of ::time-point))

(s/def ::cluster (s/coll-of ::grid-cell))

(s/def ::clusters (s/coll-of ::cluster))

(s/def ::grid-cell (s/keys :req [::position ::time-points]))

(s/def ::grid-cells (s/coll-of ::grid-cell))

(s/def ::properties (s/keys :req [::c_m
                                  ::c_l
                                  ::lambda
                                  ::beta
                                  ::dimensions
                                  ::domains
                                  ::gap_time]))

(s/def ::initialized-clusters boolean?)

(s/def ::state (s/keys :req [::clusters
                             ::grid-cells
                             ::properties
                             ::initialized-clusters]))


;;TODO create an fdef and instrument to register specs

(defn dstream [{:keys [::state]}]
  ;(println (s/explain ::state state))
  state
  )

(s/fdef dstream
        :args ::state
        :ret ::state)

(stest/instrument `dstream)
