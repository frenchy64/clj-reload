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

(comment
  (linear-fj-plan '{:to-unload (f a h d c e), :to-load (e c d h a f)})
  (linear-fj-plan '{:to-unload (i j), :to-load (j)})
  ; {i {:unload? true :load? false :load-after {j {:unload? true :load? false :load-after? nil}}}}
  (linear-fj-plan '{:to-unload (), :to-load (i)})
  (linear-fj-plan {:to-unload '[a b c d e] :to-load (rseq '[a b c d e])})
  {a
   {:unload? true,
    :load? true,
    :load-after
    {b
     {:unload? true,
      :load? true,
      :load-after
      {c
       {:unload? true,
        :load? true,
        :load-after
        {d
         {:unload? true,
          :load? true,
          :load-after
          {e {:unload? true, :load? true, :load-after nil}}}}}}}}}}
)
