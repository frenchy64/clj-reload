(ns clj-reload.plan
  (:require
    [clj-reload.parse :as parse]
    [clojure.set :as set]))

(defn topo-sort-fn
  "Accepts dependees map {ns -> #{downstream-ns ...}},
   returns a fn that topologically sorts dependencies"
  [deps]
  (let [sorted (parse/topo-sort deps)]
    (fn [coll]
      (filter (set coll) sorted))))

(defn linear-fj-plan
  [{:keys [to-unload to-load] :as state}]
  (let [unloads (set to-unload)
        loads (set to-load)
        unload-only (set/difference unloads loads)
        load-only (set/difference loads unloads)
        load+unload (set/intersection loads unloads)]
    (loop [[ul :as to-unload] (vec (reverse to-unload))
           [ld :as to-load] (vec to-load)
           plan []]
      (if (or ul ld)
        (recur (cond-> to-unload ul (subvec 1))
               (cond-> to-load ld (subvec 1))
               [(-> {}
                    (cond-> ul (assoc :before [{:op :unload :ns ul}]))
                    (assoc :forks plan)
                    (cond-> ld (assoc :after [{:op :load :ns ld}])))])
        plan))))

(defn fj-plan
  [{:keys [to-unload to-load] :as state}]
  (linear-fj-plan state))

;    a     f     i  l  m
;  ╱ │ ╲ ╱   ╲   │     │╲
; b  c  d  h  g  j     n│
;     ╲ │ ╱      │     │╱
;       e        k     o

;; 1. Unload roots: a f i l m
;;    Load leaves with all unloaded deps:
;; 2. Unload max 1 from root with all loaded dependents b c d g j n
;;    Load max 1 from leaf with all unloaded deps: l
;; 3. Unload max 2 from root with no loaded dependents: e k o
;;    Load max 2 from leaf with all unloaded deps: b g
;; 4. Unload max 3 from root with no loaded dependents: h
;;    Load max 3 from leaf with all unloaded deps: k o
;; 5. Load max 4 from leaf with all unloaded deps: e j n
;; 6. Load max 5 from leaf with all unloaded deps: c d i m
;; 7. Load max 6 from leaf with all unloaded deps: a f

(comment
  [{:before (mapv #(do {:op :unload :ns a}) [])}]
  )

;; not a great approach as it doesn't fully utilize threads. no coordination necessary though.
(defn fj-plan-rings [opts]
  )

(defn fj-fsm [{:keys [to-unload to-load unloaded-volatiles loaded-volatiles namespaces]} {:clj-reload.core/keys [cancel] :as opts}]
  (let [to-unload-set (set to-unload) ;;TODO is this the right set to filter on?
        to-load-set (set to-load)
        unloaded-volatiles (zipmap to-unload (repeatedly #(volatile! false)))
        loaded-volatiles (zipmap to-load (repeatedly #(volatile! false)))
        state (atom {:unloaded #{}
                     :loaded #{}
                     :to-unload-set to-unload-set
                     :to-load-set (set to-load)
                     :to-unload to-unload
                     :to-load to-load
                     :available []})
        dependees (parse/dependees namespaces)
        dependents (parse/dependents namespaces)
        ns->downstream (update-vals dependees
                                    (fn [immediate-dependees]
                                      (set/intersection to-unload-set
                                                        (parse/transitive-closure dependees immediate-dependees))))
        to-load-or-unload-set (set/union to-load-set to-unload-set)
        ns->upstream (update-vals dependents
                                  (fn [immediate-dependents]
                                    (set/intersection to-load-or-unload-set
                                                      (parse/transitive-closure dependents immediate-dependents))))
        ns->can-unload? (into {} (map (fn [ns]
                                        (let [downstream (ns->downstream ns)
                                              relevant-volatiles (mapv unloaded-volatiles downstream)]
                                          [ns (fn [_]
                                                (every? deref relevant-volatiles))])))
                              to-unload)
        ns->can-load? (into {} (map (fn [ns]
                                      (let [upstream (ns->upstream ns)
                                            relevant-volatiles (-> (mapv unloaded-volatiles upstream)
                                                                   (conj (unloaded-volatiles ns))
                                                                   (into (map loaded-volatiles) upstream))]
                                        [ns (fn [_]
                                              (every? deref relevant-volatiles))])))
                            to-load)
        ->s (fn ->s []
              (lazy-seq
                (loop []
                  (let [{:keys [available to-unload to-load]}
                        (swap! state
                               (fn [{:keys [to-unload to-load] :as m}]
                                 (let [can-unloads (filterv #((ns->can-unload? %) m) to-unload)
                                       can-loads (filterv #((ns->can-load? %) m) to-load)]
                                   (if-some [available (not-empty (into (mapv #(do {:op :unload :ns %}) can-unloads)
                                                                        (map #(do {:op :load :ns %}))
                                                                        can-loads))]
                                     (-> m
                                         (update :to-unload #(into [] (remove (set can-unloads)) %))
                                         (update :to-load #(into [] (remove (set can-loads)) %))
                                         (update :to-unload-set #(apply disj % can-unloads))
                                         (update :to-load-set #(apply disj % can-loads))
                                         (assoc :available available))
                                     m))))]
                    (or (when (seq available)
                          (mapv (fn [])))
                        (when (and (or (seq to-unload)
                                       (seq to-load))
                                   (not (Thread/interrupted))
                                   (not @cancel))
                          (recur)))))))
        s (->s)]
    s))

(comment
  (fj-fsm
    (#'clj-reload.core/scan @@#'clj-reload.core/*state {})
    nil
    )
  )
