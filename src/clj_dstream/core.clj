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

(s/def ::last-time-update-time int?)
(s/def ::last-time-removed-as-sporadic int?)
(s/def ::grid-density-at-last-update float?)
(s/def ::sporadic-or-normal #{::sporadic ::normal})

(s/def ::char-vec (s/keys :req [::last-update-time ::last-time-removed-as-sporadic ::grid-density-at-last-update ::sporadic-or-normal ::cluster-label]))

(s/def ::grid-cell (s/keys :req [::position-value ::position-index ::char-vec]))

(s/def ::grid-cells (s/map-of ::position-index ::char-vec))

(s/def ::gap_time pos-int?)

(s/def ::beta float?)
(s/def ::lambda float?)
(s/def ::c_l float?)
(s/def ::c_m float?)
(s/def ::N pos-int?)

(s/def ::properties (s/keys :req [::c_m
                                  ::c_l
                                  ::lambda
                                  ::beta
                                  ::dimensions
                                  ::domains
                                  ::gap_time]
                            :opt [::N]))

(s/def ::initialized-clusters boolean?)

(s/def ::state (s/keys :req [::grid-cells
                             ::properties
                             ::initialized-clusters]))


(s/def ::raw-datum (s/keys :req [::value ::position-value]))

(def test-state
  {::grid-cells           {[0 1 2 3] {::last-update-time              0
                                      ::last-time-removed-as-sporadic 0
                                      ::grid-density-at-last-update   0.2
                                      ::sporadic-or-normal            ::normal
                                      ::cluster-label                 nil}}
   ::properties           {::c_m        0.1
                           ::c_l        0.2
                           ::lambda     0.99
                           ::beta       0.223
                           ::dimensions 4
                           ::domains    [
                                         {::domain-start    -1.0
                                          ::domain-end      10.0
                                          ::domain-interval 0.1}
                                         {::domain-start    -1.0
                                          ::domain-end      10.0
                                          ::domain-interval 0.1}
                                         {::domain-start    -1.0
                                          ::domain-end      10.0
                                          ::domain-interval 0.1}
                                         {::domain-start    -1.0
                                          ::domain-end      10.0
                                          ::domain-interval 0.1}]
                           ::gap_time   4}
   ::initialized-clusters true})

(def test-raw-data
  {::value          0.1
   ::position-value [5.0 5.0 5.0 5.0]})

(defn position-value->position-index [{:keys [::position-value ::domains]}]
  (if-not (= (count position-value) (count domains))
    (throw (ex-info "Dimension mismatch" {:keys :data position-value domains})))
  (->>
    position-value
    (map-indexed (fn [idx position-val]
                   (let [{:keys [::domain-start ::domain-end ::domain-interval]} (get domains idx)]
                     (if (or (> position-val domain-end)
                             (< position-val domain-start))
                       (throw
                         (ex-info
                           "Raw data value outside range" {:keys idx
                                                           :data {:value  position-val
                                                                  :domain {::domain-interval domain-interval
                                                                           ::domain-start    domain-start
                                                                           ::domain-end      domain-end}}})))
                     (let [dist        (- position-val domain-start)
                           value-index (int (/ dist domain-interval))]
                       value-index))))
    vec))

(defn put [{:keys [::raw-datum ::state ::t]}]
  (let [idx (position-value->position-index (merge raw-datum (::properties state)))]
    (if (contains? (::grid-cells state) idx)
      (let [char-vec  (get-in state [::grid-cells idx])
            new-d     (+ 1.0
                         (*
                           (::grid-density-at-last-update char-vec)
                           (Math/pow (get-in state [::properties ::lambda])
                                     (- t
                                        (::last-update-time char-vec)))))
            new-cv    (assoc char-vec
                        ::last-update-time t
                        ::grid-density-at-last-update new-d)
            new-state (assoc-in state [::grid-cells idx] new-cv)]
        {::state new-state})
      (let [new-state (assoc-in state
                                [::grid-cells idx]
                                {::last-update-time              t
                                 ::last-time-removed-as-sporadic 0
                                 ::grid-density-at-last-update   1.0
                                 ::sporadic-or-normal            ::normal
                                 ::cluster-label                 nil})]
        {::state new-state}))))

(defn one-dstream-iteration [{:keys [::state ::raw-datum ::t] :as data}]
  (let [state-after-put (put data)]
    state-after-put))

(defn domains->cell-count [{:keys [::domains]}]
  ;;TODO
  9001)

(defn dstream-iterations [state raw-data]
  (println "iters w state:")
  (clojure.pprint/pprint state)
  (println "raw data: ")
  (clojure.pprint/pprint raw-data)
  (let [time*      (atom 0)
        the-state* (atom state)]
    (if-not (get-in state [::properties ::N])
      (do
        (reset! the-state* (assoc-in
                              @the-state*
                              [::state ::properties ::N]
                              (domains->cell-count (get-in @the-state* [::state ::properties]))))))
    (doseq [raw-datum raw-data]
      (reset! the-state* (one-dstream-iteration (merge @the-state* raw-datum {::t @time*})))
      (swap! time* inc))
    @the-state*))

(defn update-char-vec-label [{:keys [::char-vec ::properties]}]
  ;;TODO
  (let [cell-count (get-in properties [::N])
        dense-coeff (/ (::c_m properties) (* ))
        sparse-coeff ()]))

(s/fdef one-dstream-iteration
        :args (s/cat :u (s/keys :req [::state ::raw-datum]))
        :ret (s/keys :req [::state]))

(s/fdef put
        :args (s/cat :u (s/keys :req [::state ::raw-datum ::t]))
        :ret (s/keys :req [::state]))

(s/fdef position-value->position-index
        :args (s/cat :u (s/keys :req [::position-value ::domains]))
        :ret ::position-index)

(s/fdef dstream-iterations
        :args (s/cat :u (s/cat :state (s/keys :req [::state]) :raw-data (s/coll-of (s/keys :req [::raw-datum]))))
        :ret (s/keys :req [::state]))

(s/fdef update-char-vec-label
        :args (s/cat :u (s/keys :req [::char-vec ::properties]))
        :ret (s/keys :req [::char-vec]) )

(s/fdef domains->cell-count
        :args (s/cat :u (s/keys :req [::domains]))
        :ret int?)

(stest/instrument `one-dstream-iteration)
(stest/instrument `position-value->position-index)
(stest/instrument `put)
(stest/instrument `dstream-iterations)
(stest/instrument `update-char-vec-label)
(stest/instrument `domains->cell-count)

;(clojure.pprint/pprint (one-dstream-iteration {::state test-state ::raw-data test-raw-data}))