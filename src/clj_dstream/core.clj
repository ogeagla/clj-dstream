(ns clj-dstream.core
  (:gen-class)
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;TODO thought i needed to spec these "enums" for spec, but doesnt make it work
;(s/def ::dense string?)
;(s/def ::sparse string?)
;(s/def ::transitional string?)
;(s/def ::sporadic string?)
;(s/def ::normal string?)

(s/def ::cluster-label (s/or :string string? :nil nil?))

(s/def ::label #{::dense ::sparse ::transitional})

(s/def ::status #{::sporadic ::normal})

(s/def ::value float?)

(s/def ::dimensions pos-int?)

(s/def ::position-value (s/and vector?
                               ;#(= ::dimensions (count %))
                               ;#(doseq [v %] (float? v))
                               ))

(s/def ::position-index (s/and vector?
                               ;#(= ::dimensions (count %))
                               ;#(doseq [v %] (int? v))
                               ))
(s/def ::domain-start float?)
(s/def ::domain-end float?)
(s/def ::domain-interval (s/and float? pos?))
(s/def ::domain (s/keys :req [::domain-start ::domain-end ::domain-interval]))
(s/def ::domains (s/coll-of ::domain))

(s/def ::t int?)

(s/def ::time-point (s/keys :req [::t
                                  ::value
                                  ::status
                                  ::label
                                  ::cluster-label]))

(s/def ::time-points (s/coll-of ::time-point))

(s/def ::grid-cell (s/keys :req [::position-value ::position-index ::time-points]))

(s/def ::grid-cells (s/coll-of ::grid-cell))

(s/def ::gap_time pos-int?)

(s/def ::beta float?)
(s/def ::lambda float?)
(s/def ::c_l float?)
(s/def ::c_m float?)


(s/def ::properties (s/keys :req [::c_m
                                  ::c_l
                                  ::lambda
                                  ::beta
                                  ::dimensions
                                  ::domains
                                  ::gap_time]))

(s/def ::initialized-clusters boolean?)

(s/def ::state (s/keys :req [::grid-cells
                             ::properties
                             ::initialized-clusters]))


(s/def ::raw-data (s/keys :req [::value ::position-value]))

(def test-state
  {::grid-cells           [{::position-value [0.1 1.0 2.0 3.0]
                            ::position-index [0 1 2 3]
                            ::time-points    [{::t             0
                                            ::value         0.1
                                            ::status        ::sporadic
                                            ::label         ::dense
                                            ::cluster-label "C0"}]}]
   ::properties           {::c_m        0.1
                           ::c_l        0.2
                           ::lambda     0.99
                           ::beta       0.223
                           ::dimensions 4
                           ::domains    [
                                         {::domain-start    -1.0
                                          ::domain-end      1.0
                                          ::domain-interval 0.1}
                                         {::domain-start    -1.0
                                          ::domain-end      1.0
                                          ::domain-interval 0.1}
                                         {::domain-start    -1.0
                                          ::domain-end      1.0
                                          ::domain-interval 0.1}
                                         {::domain-start    -1.0
                                          ::domain-end      1.0
                                          ::domain-interval 0.1}]
                           ::gap_time   4}
   ::initialized-clusters true})

(def test-raw-data
  {::value          0.1
   ::position-value [5.0 5.0 5.0 5.0]})

(defn one-dstream-iteration [{:keys [::state ::raw-data]}]
  state)

(s/fdef one-dstream-iteration
        :args (s/cat :u (s/keys :req [::state ::raw-data]))
        :ret (s/keys :req [::state]))

(stest/instrument `one-dstream-iteration)

(clojure.pprint/pprint (one-dstream-iteration {::state test-state ::raw-data test-raw-data}))