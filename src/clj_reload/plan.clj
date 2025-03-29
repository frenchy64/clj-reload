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
  (prn state)
  ;;TODO
  (assert (= to-unload (seq (reverse to-load))))
  (when (seq to-unload)
    {(first to-unload)
     {:unload? true
      :load? true
      :load-after (linear-fj-plan (-> state
                                      (update :to-unload next)
                                      (update :to-load butlast)))}}))

(comment
  (linear-fj-plan '[a b c d e] '[a b c d e])
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
