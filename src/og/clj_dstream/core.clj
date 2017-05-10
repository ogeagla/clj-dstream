(ns og.clj-dstream.core
  (:gen-class)
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [loom.graph :as lgraph]
            [loom.alg :as lalg]
            [loom.graph :as g]
            [taoensso.timbre :as timbre
             :refer [log trace debug info warn error fatal]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;;TODO initial clustering never goes more than 2 iterations, could there be a bug in state exchanges?
;;TODO adjust-clustering every N should either be 1 by defauly (?) or computed dynamically to ensure even eg 20 data points results in some meaningful output
;;TODO more than one datum per time slice? 
;;TODO remove concept of 'NO_CLASS', as that is an impl detail from ref paper

(def do-logging (atom true))

(defn log-it [context summary data]
  (when @do-logging
    (info (hash context) summary data)))

(s/def ::grid-topo #{::inside ::outside})

(s/def ::cluster (s/or :string string? :nil nil?))

(s/def ::label #{::dense ::sparse ::transitional})

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
(s/def ::points-per-time-slice int?)

(s/def ::neighbor-dimension int?)

(s/def ::last-update-time int?)
(s/def ::last-time-removed-as-sporadic int?)
(s/def ::density-at-last-update float?)
(s/def ::sporadicity #{::sporadic ::normal})

(s/def ::char-vec (s/keys :req [::last-time-label-changed
                                ::last-update-time
                                ::last-time-removed-as-sporadic
                                ::density-at-last-update
                                ::sporadicity
                                ::cluster
                                ::label]))

(s/def ::grid-cells (s/map-of ::position-index ::char-vec))

(s/def ::gap-time pos-int?)

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
                                  ::gap-time]
                            :opt [::N]))

(s/def ::initialized-clusters boolean?)

(s/def ::state (s/keys :req [::grid-cells
                             ::properties
                             ::initialized-clusters]))

(s/def ::raw-datum (s/keys :req [::value ::position-value]))

(defn position-value->position-index [{:keys [::position-value ::phase-space]}]
  (when-not (= (count position-value) (count phase-space))
    (throw (ex-info "Dimension mismatch" {:keys :data position-value phase-space})))
  (->>
    position-value
    (map-indexed (fn [idx position-val]
                   (let [{:keys [::domain-start ::domain-end ::domain-interval]} (get phase-space idx)]
                     (when-not (or (> position-val domain-end)
                                   (< position-val domain-start))
                       (do
                         ;;TODO what do here?
                         ;; do throw exception,
                         ;; conform data into range (creating hotspots on edge?),
                         ;; or just ignore the datum?
                         (max domain-start (min domain-end position-val)))
                       #_(throw
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
                      (or (::last-update-time char-vec)
                          0.0)))))))

(defn put [{:keys [::raw-datum ::state ::t]}]
  ;(log-it raw-datum ::put-datum raw-datum)
  (let [idx (position-value->position-index (merge raw-datum (::properties state)))]
    (if (contains? (::grid-cells state) idx)
      (let [char-vec  (get-in state [::grid-cells idx])
            new-cv    (assoc (update-char-vec-density char-vec t (get-in state [::properties ::lambda]) true)
                        ::last-update-time t)
            new-state (assoc-in state [::grid-cells idx] new-cv)]
        new-state)
      (let [new-state (assoc-in state
                                [::grid-cells idx]
                                {::last-update-time              t
                                 ::last-time-removed-as-sporadic 0
                                 ::density-at-last-update        1.0
                                 ::sporadicity                   ::normal
                                 ::cluster                       nil
                                 ::label                         ::sparse
                                 ::last-time-label-changed       0})]
        new-state))))

(defn phase-space->cell-count [{:keys [::phase-space]}]
  (->>
    phase-space
    (map (fn [{:keys [::domain-interval ::domain-end ::domain-start]}]
           (-> (- domain-end domain-start)
               (/ domain-interval)
               int)))
    (reduce *)))

(defn update-char-vec-label [{:keys [::char-vec ::properties ::t]}]
  (let [{:keys [::N ::c_m ::c_l ::lambda]} properties
        {:keys [::density-at-last-update]} char-vec
        dense-coeff  (/ c_m (* N (- 1.0 lambda)))
        sparse-coeff (/ c_l (* N (- 1.0 lambda)))
        new-label    (case [(>= density-at-last-update dense-coeff)
                            (<= density-at-last-update sparse-coeff)]
                       [true false] ::dense
                       [false true] ::sparse
                       ::transitional)
        new-char-vec (-> char-vec
                         (assoc ::label new-label))]
    (if (= (::label char-vec) new-label)
      new-char-vec
      (assoc new-char-vec
        ::last-time-label-changed t)
      )))

(defn are-neighbors_unmemo [position-indices-1 position-indices-2 & [neighbor-dimension]]
  (p ::are-neighbors
     (if (= position-indices-1 position-indices-2)
       true
       (let [zipped-truthiness (->> (pmap vector position-indices-1 position-indices-2)
                                    (pmap (fn [[a b]] (= a b))))]
         (and (= 1 (count (remove true? zipped-truthiness)))
              (let [index-of-false (.indexOf zipped-truthiness false)]
                (and
                  (if neighbor-dimension
                    (= neighbor-dimension index-of-false)
                    true)
                  (not (= -1 index-of-false))
                  (= 1 (Math/abs (- (get position-indices-1 index-of-false)
                                    (get position-indices-2 index-of-false)))))))))))

(def are-neighbors (memoize are-neighbors_unmemo))

(defn pos-idxs->graph_unmemo [position-indices]
  ;;TODO spec this
  ;;TODO optimize the shit out of this function; it takes 2.5 seconds to run and is called millions of times per
  (p ::pos-idxs->graph
     (->
       (into
         {}
         (pmap
           (fn [pos-idx]
             [pos-idx
              (filter
                (fn [ref-idx]
                  (and
                    (not (= pos-idx ref-idx))
                    (are-neighbors pos-idx ref-idx)))
                position-indices)])
           position-indices))
       (lgraph/graph))))

(def pos-idxs->graph (memoize pos-idxs->graph_unmemo))

(defn is-grid-group
  "are all grids transitively neighbors?
  ie, is the graph fully connected"
  [position-indices]
  (p ::is-grid-group
     (let [results (-> (pos-idxs->graph position-indices)
                       (lalg/connected?))]
       results)))

(defn pos-is-inside-or-outside-group [grid-pos group-poss]
  (p ::pos-is-inside-or-outside-group
     (let [group-minus-grid (remove #(= grid-pos %) group-poss)
           truth-per-dim    (map-indexed
                              (fn [idx pos-at-idx]
                                (let [neighbors-in-this-dim (pmap (fn [grid-from-group]
                                                                    (are-neighbors grid-pos grid-from-group idx))
                                                                  group-minus-grid)]
                                  (some true? neighbors-in-this-dim)))
                              grid-pos)
           is-inside        (every? true? truth-per-dim)]
       (if is-inside
         ::inside
         ::outside))))

(defn is-grid-cluster [grid-cells]
  ;; is grid cluster
  (p ::is-grid-cluster
     (and (is-grid-group (keys grid-cells))
          ;;every inside grid is dense and others are dense or transitional
          (every? true?
                  (let [grid-keys (keys grid-cells)]
                    (pmap (fn [[pos-idx char-vec]]
                            (let [the-label (::label char-vec)]
                              (case (pos-is-inside-or-outside-group pos-idx grid-keys)
                                ::inside (= ::dense the-label)
                                ::outside (or (= ::dense the-label)
                                              (= ::transitional the-label)))))
                          grid-cells))))))

(defn grid-cell->grid-group [the-grid-cell all-grid-cells]
  ;;TODO spec this
  (p ::grid-cell->grid-group
     (let [g              (pos-idxs->graph (keys all-grid-cells))
           conn-comps     (lalg/connected-components g)
           conns-for-cell (first (filter (fn [adj-group]
                                           (some #(= %
                                                     (first
                                                       (keys the-grid-cell)))
                                                 adj-group))
                                         conn-comps))]
       (select-keys all-grid-cells conns-for-cell))))

(defn grid-cell->neighbors [the-grid-cell all-grid-cells]
  ;;TODO spec this
  (p ::grid-cell->neighbors
     (let [ref-idx   (first (keys the-grid-cell))
           g         (pos-idxs->graph (keys all-grid-cells))
           neighbors (get (:adj g) ref-idx)]
       (select-keys all-grid-cells neighbors))))

(defn cluster->grid-cells [cluster state]
  (p ::cluster->grid-cells
     ;;TODO spec this
     (if-not (= "NO_CLASS" cluster)
       (into {}
             (->> state
                  ::grid-cells
                  (filter (fn [[pos-idx char-vec]]
                            (= cluster (::cluster char-vec)))))))))

(defn cluster->size [cluster state]
  ;;TODO spec this
  (count
    (cluster->grid-cells cluster state)))

(defn split-cluster [grid-cells]
  (p ::split-cluster
     ;;TODO spec this
     (let [g                     (pos-idxs->graph (keys grid-cells))
           connecteds            (lalg/connected-components g)
           connected-grids-cells (map (fn [idxs]
                                        (select-keys grid-cells idxs))
                                      connecteds)
           w-new-clusters        (flatten
                                   (map
                                     (fn [the-conn-grid-cells]
                                       (if (is-grid-group (keys the-conn-grid-cells))
                                         (let [id (str (java.util.UUID/randomUUID))]
                                           (into {}
                                                 (map (fn [[pos-idx char-vec]]
                                                        [pos-idx (assoc char-vec ::cluster id)])
                                                      the-conn-grid-cells)))
                                         (recur the-conn-grid-cells)))
                                     connected-grids-cells))]
       w-new-clusters)))

(defn relabel-cluster [old-cluster-label new-cluster-label state]
  ;;TODO spec this
  (p ::relabel-cluster
     (let [grids             (cluster->grid-cells old-cluster-label state)
           grids-w-new-label (into {}
                                   (map (fn [[pos-idx char-vec]]
                                          [pos-idx (assoc char-vec
                                                     ::cluster new-cluster-label)])
                                        grids))]
       grids-w-new-label)))

(defn update-char-vec-in-state! [state* pos-idx new-char-vec]
  (p ::update-char-vec-in-state!
     (swap! state* assoc-in [::grid-cells pos-idx] new-char-vec)))

(defn update-grid-cells [state t]
  ;;TODO spec this
  (p ::update-grid-cells
     (let [lambda (get-in state [::properties ::lambda])
           state* (atom state)]
       (doseq [[pos-idx char-vec] (::grid-cells @state*)]
         (update-char-vec-in-state!
           state*
           pos-idx
           (update-char-vec-label
             {::char-vec   (update-char-vec-density char-vec t lambda false)
              ::properties (::properties state)
              ::t          t})))
       @state*)))

(defn dense-grids->unique-clusters [state]
  (let [state* (atom state)]
    (doseq [[pos-idx char-vec] (::grid-cells @state*)]
      (if (= ::dense (::label char-vec))
        (do
          (log-it state ::dense-grid-creating-cluster {:char-vec char-vec})
          (swap! state* assoc-in
                 [::grid-cells pos-idx ::cluster]
                 (str (java.util.UUID/randomUUID))))
        (do
          ;(log-it state ::not-dense-grid {:char-vec char-vec})
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
          (let [its-grid-group (grid-cell->grid-group (apply hash-map grid-in-cluster)
                                                      (::grid-cells @the-state*))
                outside-grids  (filter
                                 (fn [[pos-idx char-vec]]
                                   (= ::outside
                                      (pos-is-inside-or-outside-group pos-idx
                                                                      (keys its-grid-group))))
                                 its-grid-group)]
            (doseq [outside-grid outside-grids]
              (let [neighbors (grid-cell->neighbors (apply hash-map outside-grid)
                                                    (::grid-cells @the-state*))]
                (doseq [neighbor neighbors]
                  (let [neighbor-cluster      (::cluster (second neighbor))
                        neighbor-cluster-size (cluster->size neighbor-cluster @the-state*)
                        initial-cluster-size  (cluster->size initial-cluster @the-state*)]
                    ;;if neighbor belongs to a cluster
                    (if (< 0 neighbor-cluster-size)
                      (if (> initial-cluster-size neighbor-cluster-size)
                        (swap! the-state* assoc-in [::grid-cells]
                               (merge (::grid-cells @the-state*)
                                      (relabel-cluster neighbor-cluster initial-cluster @the-state*)))
                        (swap! the-state* assoc-in [::grid-cells]
                               (merge (::grid-cells @the-state*)
                                      (relabel-cluster initial-cluster neighbor-cluster @the-state*))))
                      ;;elif neighbor does not have a cluster
                      (when (= ::transitional (::label (second neighbor)))
                        (swap! the-state* assoc-in [::grid-cells]
                               (merge (::grid-cells @the-state*)
                                      {(first neighbor)
                                       (assoc (second neighbor) ::cluster initial-cluster)}))))))))))))
    @the-state*))

(defn- init-clustering-iterations [state iter-count]
  (log-it state ::init-clustering {:iteration iter-count})
  (let [state-after (initial-clustering-single-pass state)]
    (if (= state state-after)
      (do
        (log-it state-after ::init-clustering.complete {:iterations iter-count})
        state-after)
      (do
        (log-it state-after ::init-clustering.recurring {:iterations iter-count})
        (recur state-after (inc iter-count))))))

(defn state->clusters [state]
  "For use as external API? What would a user want when they ask for the clusters?"
  (let [clus         (->>
                       (::grid-cells state)
                       (map #(::cluster (second %)))
                       (remove #(or (nil? %)
                                    (= "NO_CLASS" %)))
                       (map (fn [cluster]
                              [cluster (cluster->grid-cells cluster state)])))
        w-grid-cells (into {}
                           clus)]
    {:clusters-grid-cells w-grid-cells
     :properties          (::properties state)}))

(defn initial-clustering [state t]
  ;;TODO spec
  (let [init-state (->
                     state
                     (update-grid-cells t)
                     dense-grids->unique-clusters)]
    (init-clustering-iterations init-state 1)))

(defn- step-nine! [current-cluster updated-state* pos-idx biggest-neighbor char-vec]
  (if (= "NO_CLASS" current-cluster)
    ;;step 10
    (do
      (log-it pos-idx ::step-nine-true biggest-neighbor)
      (update-char-vec-in-state! updated-state* pos-idx (assoc char-vec ::cluster biggest-neighbor)))
    (swap! updated-state* assoc-in [::grid-cells]
           (merge (::grid-cells @updated-state*)
                  (if (> (cluster->size current-cluster @updated-state*)
                         (cluster->size biggest-neighbor @updated-state*))
                    ;;step 11
                    (do
                      (log-it pos-idx ::move-biggest-neighboring-cluster-to-current-cluster
                              {:from biggest-neighbor
                               :to   current-cluster})
                      (relabel-cluster biggest-neighbor current-cluster @updated-state*))
                    ;;step 12
                    (do
                      (log-it pos-idx ::move-current-cluster-to-biggest-neighboring-cluster
                              {:from current-cluster
                               :to   biggest-neighbor})
                      (relabel-cluster current-cluster biggest-neighbor @updated-state*)))))))

(defn- step-four! [char-vec pos-idx updated-state*]
  (let [old-cluster (::cluster char-vec)]
    ;(log-it pos-idx ::sparse-label {:cluster old-cluster})
    (update-char-vec-in-state! updated-state* pos-idx (assoc char-vec ::cluster "NO_CLASS"))
    (let [new-cluster (cluster->grid-cells old-cluster @updated-state*)]
      ;;step 6
      (when (and
              (not (empty? new-cluster))
              (not (is-grid-group (keys new-cluster))))
        (do
          (log-it pos-idx ::split-cluster new-cluster)
          (let [split-clusters (split-cluster new-cluster)]
            (doseq [split-cluster-map split-clusters]
              (doseq [[p-i c-v] split-cluster-map]
                (update-char-vec-in-state! updated-state* p-i c-v)))))))))

(defn- step-fifteen! [current-cluster grid-w-biggest-neighbor updated-state* pos-idx char-vec biggest-neighbor]
  ;;TODO is this correct? i think outer when needs an if, as there is an else case in the paper
  (when
    (and
      (= "NO_CLASS" current-cluster)
      (= ::outside (pos-is-inside-or-outside-group
                     (first grid-w-biggest-neighbor)
                     (keys
                       (merge
                         (cluster->grid-cells biggest-neighbor @updated-state*)
                         {pos-idx char-vec})))))
    (update-char-vec-in-state!
      updated-state*
      pos-idx
      (assoc
        char-vec
        ::cluster
        biggest-neighbor)))
  (when (and (not (= "NO_CLASS" current-cluster))
             (not (= nil current-cluster))
             (>= (cluster->size current-cluster @updated-state*)
                 (cluster->size biggest-neighbor @updated-state*)))
    (update-char-vec-in-state!
      updated-state*
      (first (keys grid-w-biggest-neighbor))
      (assoc
        (first (vals grid-w-biggest-neighbor))
        ::cluster
        current-cluster))))

(defn adjust-clustering [state t]
  (log-it [state t] ::adjust-clustering [{:t t}])
  (let [updated-state* (atom (update-grid-cells state t))]
    (doseq [[pos-idx char-vec] (::grid-cells @updated-state*)]
      ;(log-it pos-idx ::adjust-clustering-for-char-vec [{:t t} char-vec])
      (when (= t (::last-time-label-changed char-vec))
        (do
          ;(log-it pos-idx ::label-changed-this-iteration char-vec)
          (cond
            ;;step 4
            (= ::sparse (::label char-vec)) (p ::step-four (step-four! char-vec pos-idx updated-state*))

            ;;step 7
            (= ::dense (::label char-vec)) (p ::step-seven
                                              (do
                                                (log-it pos-idx ::step-seven-dense-label char-vec)
                                                (let [neighbors          (grid-cell->neighbors {pos-idx char-vec} (::grid-cells @updated-state*))
                                                      neighbors-clusters (->>
                                                                           neighbors
                                                                           vals
                                                                           (map ::cluster)
                                                                           (remove nil?)
                                                                           not-empty)]
                                                  (when neighbors-clusters
                                                    (let [biggest-neighbor        (->>
                                                                                    neighbors-clusters
                                                                                    (pmap (fn [cluster]
                                                                                            hash-map
                                                                                            :cluster cluster
                                                                                            :cluster-size (cluster->size
                                                                                                            cluster
                                                                                                            @updated-state*)))
                                                                                    (sort-by :cluster-size)
                                                                                    first
                                                                                    :cluster)
                                                          grid-w-biggest-neighbor (first
                                                                                    (keep
                                                                                      (fn [[neigh-pos-id neigh-char-vec]]
                                                                                        (if (= biggest-neighbor
                                                                                               (:cluster neigh-char-vec))
                                                                                          (hash-map neigh-pos-id neigh-char-vec)))
                                                                                      neighbors))
                                                          current-cluster         (::cluster char-vec)
                                                          neighbor-label          (::label (first (vals grid-w-biggest-neighbor)))]
                                                      (case neighbor-label
                                                        ;;step 9
                                                        ::dense
                                                        (p ::step-nine (step-nine! current-cluster updated-state* pos-idx biggest-neighbor char-vec))
                                                        ;;step 15
                                                        ::transitional
                                                        (p ::step-fifteen (step-fifteen! current-cluster grid-w-biggest-neighbor updated-state* pos-idx char-vec biggest-neighbor))
                                                        ::sparse nil))))))

            )

          ;;step 19
          (p ::step-nineteen
             (when (= ::transitional (::label char-vec))
               (let [neighbors          (grid-cell->neighbors {pos-idx char-vec} (::grid-cells @updated-state*))
                     neighbors-clusters (->>
                                          neighbors
                                          vals
                                          (map ::cluster)
                                          (remove nil?)
                                          not-empty)]
                 (when neighbors-clusters
                   (let [n-clusters-w-grid-added (pmap (fn [neighbors-cluster]
                                                         (let [neighbors-cluster-w-grid (merge (cluster->grid-cells neighbors-cluster @updated-state*)
                                                                                               {pos-idx char-vec})
                                                               is-outside               (= ::outside (pos-is-inside-or-outside-group pos-idx (map first neighbors-cluster-w-grid)))
                                                               cluster-size             (cluster->size neighbors-cluster @updated-state*)]
                                                           {:is-outside   is-outside
                                                            :cluster-size cluster-size
                                                            :cluster      neighbors-cluster}))
                                                       neighbors-clusters)
                         biggest                 (->>
                                                   n-clusters-w-grid-added
                                                   (remove #(not (:is-outside %)))
                                                   (sort-by :cluster-size)
                                                   last)]
                     (when biggest
                       (do
                         (log-it biggest ::transitional-to-neigboring-cluster biggest)
                         (update-char-vec-in-state! updated-state* pos-idx (assoc char-vec ::cluster (:cluster biggest)))))))))))))
    @updated-state*))

(defn detect-and-remove-sporadic-grids [state t]
  ;;TODO spec this

  (let [state*         (atom state)
        new-grid-cells (into {}
                             (remove (fn [[pos-idx char-vec]]
                                       (= ::sporadic (::sporadicity char-vec))
                                       ) (::grid-cells state)))]
    (swap! state* assoc-in [::grid-cells] new-grid-cells)
    @state*))

(defn properties->gap-time [properties]
  (let [N          (::N properties)
        c_l        (::c_l properties)
        c_m        (::c_m properties)
        lambda     (::lambda properties)
        max-result (max (/ c_l c_m)
                        (/ (- N c_m)
                           (- N c_l)))
        value      (/ (Math/log max-result)
                      (Math/log lambda))
        gap-time   (Math/floor value)]
    (log-it properties ::gap-time gap-time)
    gap-time))

(defn one-dstream-update [{:keys [::state ::raw-datum ::t] :as data}]
  (let [state*   (atom state)
        gap-time (::gap-time (::properties state))]
    (when (= gap-time t)
      (reset! state* (p ::initial-clustering (initial-clustering @state* t))))
    (when (= 0 (mod t gap-time))
      (reset! state* (p ::detect-and-remove-sporadic-grids (detect-and-remove-sporadic-grids @state* t)))
      (reset! state* (p ::adjust-clustering (adjust-clustering @state* t))))
    @state*))

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

(s/fdef pos-is-inside-or-outside-group
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

(s/fdef one-dstream-update
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
        :args (s/cat :u (s/keys :req [::char-vec ::properties ::t]))
        :ret (s/keys :req [::char-vec]))

(s/fdef phase-space->cell-count
        :args (s/cat :u (s/keys :req [::phase-space]))
        :ret int?)

(defn instrument-specs! []
  (stest/instrument `one-dstream-update)
  (stest/instrument `position-value->position-index)
  (stest/instrument `put)
  (stest/instrument `dstream-iterations)
  (stest/instrument `update-char-vec-label)
  (stest/instrument `phase-space->cell-count)
  ;;TODO make this spec work
  ;(stest/instrument `are-neighbors)
  (stest/instrument `is-grid-group)
  (stest/instrument `pos-is-inside-or-outside-group)
  (stest/instrument `is-grid-cluster)
  (stest/instrument `initial-clustering)
  (stest/instrument `update-char-vec-density))

(instrument-specs!)

(defn dstream-iterations [{:keys [::state]} raw-data & {:keys [state-append-every
                                                               instrument-spec
                                                               data-per-time-interval]
                                                        :or   {data-per-time-interval 1}}]
  (log-it [state raw-data] ::dstream-iterations.starting {:raw-data-count   (count raw-data)
                                                          :state-properties (::properties state)})
  (when instrument-spec
    (instrument-specs!))
  (let [time*           (atom 0)
        the-state*      (atom state)
        state-appender* (atom {})]
    (if-not (get-in state [::properties ::N])
      (do
        (swap! the-state* assoc-in
               [::properties ::N]
               (phase-space->cell-count
                 (get-in @the-state* [::properties])))))
    (if-not (get-in state [::properties ::gap-time])
      (do
        (swap! the-state* assoc-in
               [::properties ::gap-time]
               (properties->gap-time (::properties @the-state*)))))
    (doseq [{:keys [::raw-datum]} raw-data]
      (if (and state-append-every
               (= 0 (mod @time* state-append-every)))
        (swap! state-appender* assoc @time* @the-state*))
      (if (= 0 (mod @time* (int (/ (count raw-data) 100))))
        (log-it raw-datum ::put-datum [{:t @time*}
                                       {:cluster-count (count
                                                         (remove #(or (nil? %) (= "NO_CLASS" %))
                                                                 (distinct
                                                                   (map ::cluster
                                                                        (map second (::grid-cells @the-state*))))))
                                        :grid-count    (count (::grid-cells @the-state*))
                                        :N             (::N (::properties @the-state*))}]))
      (let [the-data {::state     @the-state*
                      ::raw-datum raw-datum
                      ::t         @time*}]
        (reset! the-state* (put the-data))
        (if (= 0 (mod @time* data-per-time-interval))
          (do
            (reset! the-state*
                    (p ::one-dstream-iteration
                       (one-dstream-update the-data))))))
      (swap! time* inc))
    {:final-state   @the-state*
     :initial-state state
     :state-ts      @state-appender*}))