(ns clj-dstream.core
  (:gen-class)
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [loom.graph :as lgraph]
            [loom.alg :as lalg]
            [loom.graph :as g]
            [taoensso.timbre :as timbre
             :refer [log trace debug info warn error fatal report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn my-log [context summary data]
  (info (hash context) summary data))

;;;TODO thought i needed to spec these "enums" for spec, but doesnt make it work
;(s/def ::dense string?)
;(s/def ::sparse string?)
;(s/def ::transitional string?)
;(s/def ::sporadic string?)
;(s/def ::normal string?)

(s/def ::grid-topo #{::inside ::outside})

(s/def ::cluster (s/or :string string? :nil nil?))

(s/def ::label #{::dense ::sparse ::transitional})

(s/def ::status #{::sporadic ::normal})

(s/def ::value float?)

(s/def ::dimensions pos-int?)

(s/def ::position-value (s/coll-of float? :kind vector?))
(s/def ::position-index (s/coll-of int? :kind vector?))

(s/def ::domain-start float?)
(s/def ::domain-end float?)
(s/def ::domain-interval (s/and float? pos?))
(s/def ::domain (s/keys :req [::domain-start ::domain-end ::domain-interval]))
(s/def ::phase-space (s/coll-of ::domain))

(s/def ::t int?)

(s/def ::neighbor-dimension int?)

(s/def ::last-update-time int?)
(s/def ::last-time-removed-as-sporadic int?)
(s/def ::density-at-last-update float?)
(s/def ::sporadicity #{::sporadic ::normal})

(s/def ::char-vec (s/keys :req [::last-update-time
                                ::last-time-removed-as-sporadic
                                ::density-at-last-update
                                ::sporadicity
                                ::cluster
                                ::label]))

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
                                  ::phase-space
                                  ::gap_time]
                            :opt [::N]))

(s/def ::initialized-clusters boolean?)

(s/def ::state (s/keys :req [::grid-cells
                             ::properties
                             ::initialized-clusters]))


(s/def ::raw-datum (s/keys :req [::value ::position-value]))

(defn position-value->position-index [{:keys [::position-value ::phase-space]}]
  (if-not (= (count position-value) (count phase-space))
    (throw (ex-info "Dimension mismatch" {:keys :data position-value phase-space})))
  (->>
    position-value
    (map-indexed (fn [idx position-val]
                   (let [{:keys [::domain-start ::domain-end ::domain-interval]} (get phase-space idx)]
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

(defn update-char-vec-density [char-vec t lambda with-data]
  (assoc char-vec
    ::density-at-last-update
    (+ (if with-data
         1.0
         0.0)
       (*
         (::density-at-last-update char-vec)
         (Math/pow lambda
                   (- t
                      (::last-update-time char-vec)))))))

(defn put [{:keys [::raw-datum ::state ::t]}]
  (let [idx (position-value->position-index (merge raw-datum (::properties state)))]
    (if (contains? (::grid-cells state) idx)
      (let [char-vec  (get-in state [::grid-cells idx])
            new-cv    (assoc (update-char-vec-density char-vec t (get-in state [::properties ::lambda]) true)
                        ::last-update-time t)
            new-state (assoc-in state [::grid-cells idx] new-cv)]
        {::state new-state})
      (let [new-state (assoc-in state
                                [::grid-cells idx]
                                {::last-update-time              t
                                 ::last-time-removed-as-sporadic 0
                                 ::density-at-last-update        1.0
                                 ::sporadicity                   ::normal
                                 ::cluster                       nil
                                 ::label                         ::sparse})]
        {::state new-state}))))

(defn one-dstream-iteration [{:keys [::state ::raw-datum ::t] :as data}]
  (let [state-after-put (put data)]
    state-after-put))

(defn phase-space->cell-count [{:keys [::phase-space]}]
  (->>
    phase-space
    (map (fn [{:keys [::domain-interval ::domain-end ::domain-start]}]
           (-> (- domain-end domain-start)
               (/ domain-interval)
               int)))
    (reduce *)))

(defn dstream-iterations [state raw-data]
  (my-log [state raw-data] ::dstream-iterations.starting {:raw-data-count   (count raw-data)
                                                          :state-properties (::state (::properties state))})
  (let [time*      (atom 0)
        the-state* (atom state)]
    (if-not (get-in state [::properties ::N])
      (do
        (swap! the-state* assoc-in
               [::state ::properties ::N]
               (phase-space->cell-count
                 (get-in @the-state* [::state ::properties])))))
    (doseq [raw-datum raw-data]
      (reset! the-state*
              (one-dstream-iteration
                (merge @the-state* raw-datum {::t @time*})))
      (swap! time* inc))
    @the-state*))

(defn update-char-vec-label [{:keys [::char-vec ::properties]}]
  (let [{:keys [::N ::c_m ::c_l ::lambda]} properties
        {:keys [::density-at-last-update]} char-vec
        dense-coeff  (/ c_m (* N (- 1.0 lambda)))
        sparse-coeff (/ c_l (* N (- 1.0 lambda)))
        new-label    (case [(>= density-at-last-update dense-coeff)
                            (<= density-at-last-update sparse-coeff)]
                       [true false] ::dense
                       [false true] ::sparse
                       ::transitional)]
    (assoc char-vec ::label new-label)))

(defn are-neighbors [position-indices-1 position-indices-2 & [neighbor-dimension]]
  (if (= position-indices-1 position-indices-2)
    true
    (let [zipped-truthiness (->> (map vector position-indices-1 position-indices-2)
                                 (map (fn [[a b]] (= a b))))]
      (and (= 1 (count (remove true? zipped-truthiness)))
           (let [index-of-false (.indexOf zipped-truthiness false)]
             (and
               (if neighbor-dimension
                 (= neighbor-dimension index-of-false)
                 true)
               (not (= -1 index-of-false))
               (= 1 (Math/abs (- (get position-indices-1 index-of-false)
                                 (get position-indices-2 index-of-false))))))))))

(defn pos-idxs->graph [position-indices]
  ;;TODO spec this
  (-> (into {}
            (map (fn [pos-idx]
                   [pos-idx (filter
                              (fn [ref-idx]
                                (and (not (= pos-idx ref-idx))
                                     (are-neighbors pos-idx ref-idx)))
                              position-indices)])
                 position-indices))
      (lgraph/graph)))

(defn is-grid-group
  "are all grids transitively neighbors?
  ie, is the graph fully connected"
  [position-indices]
  (let [results (-> (pos-idxs->graph position-indices)
                    (lalg/connected?))]
    results))

(defn grid-is-inside-or-outside-group [grid group]
  (let [truth-per-dim (map-indexed
                        (fn [idx pos-at-idx]
                          (let [group-minus-grid      (remove #(= grid %) group)
                                neighbors-in-this-dim (map (fn [grid-from-group]
                                                             (are-neighbors grid grid-from-group idx))
                                                           group-minus-grid)]
                            (some true? neighbors-in-this-dim)))
                        grid)
        is-inside     (every? true? truth-per-dim)]
    (if is-inside
      ::inside
      ::outside)))

(defn is-grid-cluster [grid-cells]
  ;; is grid cluster
  (and (is-grid-group (keys grid-cells))
       ;;every inside grid is dense and others are dense or transitional
       (every? true?
               (let [grid-keys (keys grid-cells)]
                 (map (fn [[pos-idx char-vec]]
                        (let [the-label (::label char-vec)]
                          (case (grid-is-inside-or-outside-group pos-idx grid-keys)
                            ::inside (= ::dense the-label)
                            ::outside (or (= ::dense the-label)
                                          (= ::transitional the-label)))))
                      grid-cells)))))

(defn grid-cell->grid-group [the-grid-cell all-grid-cells]
  ;;TODO spec this
  (let [g              (pos-idxs->graph (keys all-grid-cells))
        conn-comps     (lalg/connected-components g)
        conns-for-cell (first (filter (fn [adj-group]
                                        (some #(= % (first (keys the-grid-cell))) adj-group))
                                      conn-comps))]
    (select-keys all-grid-cells conns-for-cell)))

(defn grid-cell->neighbors [the-grid-cell all-grid-cells]
  ;;TODO spec this
  (let [ref-idx   (first (keys the-grid-cell))
        g         (pos-idxs->graph (keys all-grid-cells))
        neighbors (get (:adj g) ref-idx)]
    (select-keys all-grid-cells neighbors)))

(defn cluster->grid-cells [cluster state]
  ;;TODO spec this
  (if-not (= "NO_CLASS" cluster)
    (->> state
         ::grid-cells
         (filter (fn [[pos-idx char-vec]]
                   (= cluster (::cluster char-vec)))))))

(defn cluster->size [cluster state]
  ;;TODO spec this
  (count
    (cluster->grid-cells cluster state)))

(defn relabel-cluster [old-cluster-label new-cluster-label state]
  ;;TODO spec this
  (let [grids             (cluster->grid-cells old-cluster-label state)
        grids-w-new-label (into {}
                                (map (fn [[pos-idx char-vec]] [pos-idx (assoc char-vec ::cluster new-cluster-label)]) grids))]
    grids-w-new-label))

(defn update-grid-cells [state t]
  ;;TODO spec this
  (let [lambda (get-in state [::properties ::lambda])
        state* (atom state)]
    (doseq [[pos-idx char-vec] (::grid-cells @state*)]
      (swap! state* assoc-in
             [::grid-cells pos-idx]
             (update-char-vec-label
               {::char-vec   (update-char-vec-density char-vec t lambda false)
                ::properties (::properties state)})))
    @state*))

(defn dense-grids->unique-clusters [state]
  (let [state* (atom state)]
    (doseq [[pos-idx char-vec] (::grid-cells @state*)]
      (if (= ::dense (::label char-vec))
        (do
          (my-log state ::dense-grid-creating-cluster {:char-vec char-vec})
          (swap! state* assoc-in
                 [::grid-cells pos-idx ::cluster]
                 (str (java.util.UUID/randomUUID))))
        (do (my-log state ::not-dense-grid {:char-vec char-vec})
            (swap! state* assoc-in
                   [::grid-cells pos-idx ::cluster]
                   "NO_CLASS"))))
    @state*))

(defn- initial-clustering-single-pass [the-state]
  (let [the-clusters (->> the-state
                          ::grid-cells
                          vals
                          (map ::cluster)
                          (remove nil?)
                          (remove #(= "NO_CLASS" %)))
        the-state*   (atom the-state)]
    (doseq [initial-cluster the-clusters]
      (let [grids-in-cluster (cluster->grid-cells initial-cluster @the-state*)]
        (doseq [grid-in-cluster grids-in-cluster]
          (let [its-grid-group (grid-cell->grid-group (apply hash-map grid-in-cluster) (::grid-cells @the-state*))
                outside-grids  (filter
                                 (fn [[pos-idx char-vec]]
                                   (= ::outside
                                      (grid-is-inside-or-outside-group pos-idx (keys its-grid-group)))) its-grid-group)]
            (doseq [outside-grid outside-grids]
              (let [neighbors (grid-cell->neighbors (apply hash-map outside-grid) (::grid-cells @the-state*))]
                (doseq [neighbor neighbors]
                  (let [neighbor-cluster      (::cluster (second neighbor))
                        neighbor-cluster-size (cluster->size neighbor-cluster @the-state*)
                        initial-cluster-size  (cluster->size initial-cluster @the-state*)]
                    ;;if neighbor belongs to a cluster
                    (if (< 0 neighbor-cluster-size)
                      (if (> initial-cluster-size neighbor-cluster-size)
                        (swap! the-state* assoc-in [::grid-cells] (merge (::grid-cells @the-state*)
                                                                         (relabel-cluster neighbor-cluster initial-cluster @the-state*)))
                        (swap! the-state* assoc-in [::grid-cells] (merge (::grid-cells @the-state*)
                                                                         (relabel-cluster initial-cluster neighbor-cluster @the-state*))))
                      ;;elif neighbor does not have a cluster
                      (when (= ::transitional (::label (second neighbor)))
                        (swap! the-state* assoc-in [::grid-cells] (merge (::grid-cells @the-state*)
                                                                         {(first neighbor) (assoc (second neighbor) ::cluster initial-cluster)}))))))))))))
    @the-state*))

(defn- init-clustering-iterations [state iter-count]
  (my-log state ::init-clustering {:iteration iter-count})
  (let [state-after (initial-clustering-single-pass state)]
    (if (= state state-after)
      (do
        (my-log state-after ::init-clustering.complete {:iterations iter-count})
        state-after)
      (do
        (my-log state-after ::init-clustering.recurring {:iterations iter-count})
        (recur state-after (inc iter-count))))))

(defn initial-clustering [state t]
  (let [init-state (->
                     state
                     (update-grid-cells t)
                     dense-grids->unique-clusters)]
    (init-clustering-iterations init-state 1)))


(s/fdef initial-clustering
        :args (s/cat :u ::state :v ::t)
        :ret ::state)

(s/fdef update-char-vec-density
        :args (s/cat :u ::char-vec
                     :v ::t
                     :t ::lambda
                     :w boolean?)
        :ret ::char-vec)

(s/fdef is-grid-cluster
        :args (s/cat :u ::grid-cells)
        :ret boolean?)

(s/fdef grid-is-inside-or-outside-group
        :args (s/cat :u (s/cat :grid ::position-index
                               :group (s/coll-of ::position-index)))
        :ret ::grid-topo)

(s/fdef is-grid-group
        :args (s/cat :u (s/cat :indices (s/coll-of ::position-index)))
        :ret boolean?)

(s/fdef are-neighbors
        ;;TODO this doesnt work with optional args
        :args (s/cat :u (s/cat :indices-1 ::position-index
                               :indices-2 ::position-index
                               :pos-dim (s/keys :opt [::neighbor-dimension])))
        :ret boolean?)

(s/fdef one-dstream-iteration
        :args (s/cat :u (s/keys :req [::state ::raw-datum]))
        :ret (s/keys :req [::state]))

(s/fdef put
        :args (s/cat :u (s/keys :req [::state ::raw-datum ::t]))
        :ret (s/keys :req [::state]))

(s/fdef position-value->position-index
        :args (s/cat :u (s/keys :req [::position-value ::phase-space]))
        :ret ::position-index)

(s/fdef dstream-iterations
        :args (s/cat :u (s/cat
                          :state (s/keys :req [::state])
                          :raw-data (s/coll-of (s/keys :req [::raw-datum]))))
        :ret (s/keys :req [::state]))

(s/fdef update-char-vec-label
        :args (s/cat :u (s/keys :req [::char-vec ::properties]))
        :ret (s/keys :req [::char-vec]))

(s/fdef phase-space->cell-count
        :args (s/cat :u (s/keys :req [::phase-space]))
        :ret int?)

(stest/instrument `one-dstream-iteration)
(stest/instrument `position-value->position-index)
(stest/instrument `put)
(stest/instrument `dstream-iterations)
(stest/instrument `update-char-vec-label)
(stest/instrument `phase-space->cell-count)
;;TODO make this spec work
;(stest/instrument `are-neighbors)
(stest/instrument `is-grid-group)
(stest/instrument `grid-is-inside-or-outside-group)
(stest/instrument `is-grid-cluster)
(stest/instrument `initial-clustering)
(stest/instrument `update-char-vec-density)

(def test-state
  {::grid-cells           {[10 1 2 2] {::last-update-time              0
                                       ::last-time-removed-as-sporadic 0
                                       ::density-at-last-update        0.13
                                       ::sporadicity                   ::normal
                                       ::cluster                       nil
                                       ::label                         ::sparse}
                           [10 1 2 3] {::last-update-time              0
                                       ::last-time-removed-as-sporadic 0
                                       ::density-at-last-update        0.3
                                       ::sporadicity                   ::normal
                                       ::cluster                       nil
                                       ::label                         ::sparse}

                           [0 1 2 3]  {::last-update-time              0
                                       ::last-time-removed-as-sporadic 0
                                       ::density-at-last-update        0.11
                                       ::sporadicity                   ::normal
                                       ::cluster                       nil
                                       ::label                         ::sparse}
                           [0 1 2 4]  {::last-update-time              0
                                       ::last-time-removed-as-sporadic 0
                                       ::density-at-last-update        0.02
                                       ::sporadicity                   ::normal
                                       ::cluster                       nil
                                       ::label                         ::sparse}
                           [0 1 2 5]  {::last-update-time              0
                                       ::last-time-removed-as-sporadic 0
                                       ::density-at-last-update        0.55
                                       ::sporadicity                   ::normal
                                       ::cluster                       nil
                                       ::label                         ::sparse}
                           [0 1 3 5]  {::last-update-time              0
                                       ::last-time-removed-as-sporadic 0
                                       ::density-at-last-update        0.66
                                       ::sporadicity                   ::normal
                                       ::cluster                       nil
                                       ::label                         ::sparse}
                           }
   ::properties           {::N           10000
                           ::c_m         3.0
                           ::c_l         0.8
                           ::lambda      0.998
                           ::beta        0.3
                           ::dimensions  4
                           ::phase-space [
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}
                                          {::domain-start    0.0
                                           ::domain-end      1.0
                                           ::domain-interval 0.1}]
                           ::gap_time    4}
   ::initialized-clusters true})

(def test-raw-data
  {::value          0.1
   ::position-value [0.5 0.5 0.5 0.5]})
