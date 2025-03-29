(ns clj-reload.plan
  (:require
    [clj-reload.parse :as parse]))

(defn topo-sort-fn
  "Accepts dependees map {ns -> #{downstream-ns ...}},
   returns a fn that topologically sorts dependencies"
  [deps]
  (let [sorted (parse/topo-sort deps)]
    (fn [coll]
      (filter (set coll) sorted))))

(defn linear-fj-plan
  [to-load]
  (when-some [to-load (seq to-load)]
    {(first to-load) (linear-fj-plan (next to-load))}))

(comment
  (linear-fj-plan '[a b c d e])
  '[a [b c d e]]
  '[a [b [c d e]]]
  '[a [b [c [d [e]]]]]
  )
