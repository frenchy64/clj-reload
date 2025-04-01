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

(defn fj-fsm [{:keys [to-unload to-load namespaces]} opts]
  (let [_ (prn namespaces)
        _ (prn deps)
        state (atom {:unloaded #{}
                     :loaded #{}
                     :to-unload to-unload
                     :to-load to-load})
        dependees (parse/dependees namespaces)
        dependents (parse/dependents namespaces)
        to-unload-set (set to-unload) ;;TODO is this the right set to filter on?
        downstream (update-vals dependees
                                (fn [immediate-dependees]
                                  (set/intersection to-unload-set
                                                    (parse/transitive-closure dependees immediate-dependees))))
        to-load-set (set to-load)
        to-load-or-unload-set (set/union to-load-set to-unload-set)
        upstream (update-vals dependents
                              (fn [immediate-dependents]
                                (set/intersection to-load-or-unload-set
                                                  (parse/transitive-closure dependents immediate-dependents))))
        needed-before-unload (into {} (map (fn [ns]
                                             (let [requires (get-in namespaces [ns :requires])]
                                               [ns (fn []
                                                     (let [{:keys [unloaded]} @state]
                                                       (every? unloaded requires)))])))
                                   to-unload)
        needed-before-load (into {} (map (fn [ns]
                                           (let [requires (get-in namespaces [ns :requires])]
                                             [ns (fn []
                                                   (let [{:keys [unloaded]} @state]
                                                     (every? unloaded requires)))])))
                                 to-load)
        next-state (fn [completed-op ns]
                     (case completed-op
                       :loaded (swap! state )))
        ->s (fn []
              )
        s (lazy-seq
            (->s))]
    ))

(comment
  (fj-fsm
    (#'clj-reload.core/scan @@#'clj-reload.core/*state {})
    nil
    )
  )
