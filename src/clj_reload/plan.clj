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
           plan nil]
      (cond
        (and (empty? to-unload) (empty? to-load)) plan
        (= ul ld) (recur (subvec to-unload 1) (subvec to-load 1)
                         {ul {:load? true :unload? true :load-after plan}})
        (load-only ld) (recur to-unload (subvec to-load 1)
                              {ld {:load? true :unload? false :load-after plan}})
        (unload-only ul) (recur (subvec to-unload 1) to-load
                                {ul {:load? false :unload? true :load-after plan}})
        :else (throw (ex-info "unexpected plan" (select-keys state [:to-unload :to-load])))))))

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
