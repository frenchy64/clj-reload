(ns clj-reload.core
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io File PushbackReader]))

(def ^:dynamic *log-fn*
  println)

(def ^:dynamic *stable?*
  false)

; {:dirs      [<string> ...]
;  :no-unload #{<symbol> ...}
;  :no-load   #{<symbol> ...}
;  :since     <long>
;  :files     {<file> -> File}
;  :unload    #{<symbol> ...}
;  :load      #{<symbol> ...}}
;
;  File :: {:modified   <long>
;           :namespaces {<symbol> -> Namespace}}
;
;  Namespace :: {:depends #{<symbol> ...}}

(def ^:private *state
  (atom {}))

(def ^:private reader-opts
  {:read-cond :allow
   :features  #{:clj}})

(defn- update!
  ([m k f]
   (assoc! m k (f (m k))))
  ([m k f x]
   (assoc! m k (f (m k) x)))
  ([m k f x y]
   (assoc! m k (f (m k) x y)))
  ([m k f x y & args]
   (assoc! m k (apply f (m k) x y args))))

(def ^:private conjs
  (fnil conj #{}))

(def ^:private intos
  (fnil into #{}))

(defn- now []
  (System/currentTimeMillis))

(defn- last-modified [^File f]
  (some-> f .lastModified))

(defn- file? [^File f]
  (some-> f .isFile))

(defn- file-name [^File f]
  (some-> f .getName))

(defn- reader ^PushbackReader [f]
  (PushbackReader. (io/reader (io/file f))))

(defn- expand-quotes [form]
  (walk/postwalk
    #(if (and (seq? %)
              (= 2 (count %))
              (= 'quote (first %)))
       (second %)
       %)
    form))

(defn parse-require-form [form]
  (loop [body   (next form)
         result #{}]
    (if (empty? body)
      result
      (let [[decl & body'] body]
        (cond
          (symbol? decl) ;; a.b.c
          (recur body' (conj result decl))

          (not (sequential? decl))
          (do
            (println "Unexpected" (first form) "form:" (pr-str decl))
            (recur body' result))

          (not (symbol? (first decl)))
          (do
            (println "Unexpected" (first form) "form:" (pr-str decl))
            (recur body' result))

          (or
            (nil? (second decl))      ;; [a.b.d]
            (keyword? (second decl))) ;; [a.b.e :as e]
          (recur body' (conj result (first decl)))

          :else ;; [a.b f [g :as g]]
          (let [prefix   (first decl)]
            (recur body' (into result
                               (comp ;; suffixes
                                     (map #(if (symbol? %) % (first %)) (next decl))
                                     (map #(symbol (str (name prefix) "." (name %)))))
                               (next decl)))))))))

(defn parse-ns-form [form]
  (let [name (second form)
        _ (assert (simple-symbol? name) (str "Invalid ns form: " (pr-str form)))
        body (loop [body   (nnext form)
                    result {}]
               (if (empty? body)
                 result
                 (let [[form & body'] body
                       tag  (when (list? form)
                              (first form))]
                   (if (#{:require :use} tag)
                     (recur body' (update result :depends intos (parse-require-form form)))
                     (recur body' result)))))]
    [name body]))

(defn read-file
  "Returns {<symbol> -> Namespace}"
  [rdr]
  (let [eof (Object.)
        reader-opts (assoc reader-opts :eof eof)]
    (loop [current-ns nil
           result     {}]
      (if (identical? eof form)
        result
        (let [form (binding [*read-eval* false]
                     (read reader-opts rdr))
              tag  (when (seq? form)
                     (first form))]
          (case tag
            ns
            (let [[name body] (parse-ns-form form)]
              (recur name (assoc result name body)))

            in-ns
            (let [_        (assert (= 2 (count form)) (str "Invalid in-ns form: " (pr-str form)))
                  [_ name] (expand-quotes form)]
              (assert (simple-symbol? name) (str "Invalid in-ns form: " (pr-str form)))
              (recur name result))

            (require use)
            (let [_    (assert current-ns (str "Unexpected " tag " form outside of ns"))
                  deps (parse-require-form (expand-quotes form))]
              (recur current-ns (update result current-ns update :depends intos deps)))

            (recur current-ns result)))))))

(defn find-files [dirs since]
  (sequence
    (comp (mapcat #(file-seq (io/file %)))
          (filter file?)
          (filter #(re-matches #".*\.cljc?" (file-name %)))
          (filter #(> (last-modified %) (or since 0))))
    dirs))

(defn dependees 
  "ns -> #{downstream-ns ...}"
  [files]
  (->> files
       (reduce (fn [acc [_ {nses :namespaces}]]
                 (reduce (fn [acc [from {tos :depends}]]
                           (reduce (fn [acc to]
                                     (update! acc to conjs from))
                                   (update! acc from #(or % #{}))
                                   tos))
                         acc
                         nses))
               (transient {}))
       persistent!))

(defn transitive-closure
  "Starts from starts, expands using dependees {ns -> #{downstream-ns ...}},
   returns #{ns ...}"
  [deps starts]
  (loop [queue starts
         acc   (transient #{})]
    (if (empty? queue)
      (persistent! acc)
      (let [[start & queue'] queue]
        (if (contains? acc start)
          (recur queue' acc)
          (recur (into queue (deps start)) (conj! acc start)))))))

(defn topo-sort-fn
  "Accepts dependees map {ns -> #{downstream-ns ...}},
   returns a fn that topologically sorts dependencies"
  [deps]
  (let [sorted (loop [res  (transient [])
                      deps deps]
                 (if (empty? deps)
                   (persistent! res)
                   (let [root (fn [node]
                                (when (not-any? #(% node) (vals deps))
                                  node))
                         node (if *stable?*
                                (->> (keys deps) (filter root) (sort) (first))
                                (->> (keys deps) (some root)))]
                     (recur (conj! res node) (dissoc deps node)))))]
    (fn [coll]
      (filter (set coll) sorted))))

(defn changed-files
  "Returns {<file> -> File}"
  [dirs since]
  (->> (find-files dirs since)
       (reduce
         (fn [acc changed-file]
           (assoc! acc changed-file
                   {:modified (last-modified changed-file)
                    :namespaces
                    (with-open [rdr (reader changed-file)]
                      (try
                        (read-file rdr)
                        (catch Exception e
                          (printf "Failed to load %s: %s\n" (.getPath ^File changed-file) (.getMessage e)))))}))
         (transient {}))
       persistent!))

(defn- init-impl [opts]
  (let [dirs  (vec (:dirs opts))
        now   (now)
        files (changed-files dirs 0)]
    {:dirs      dirs
     :no-unload (set (:no-unload opts))
     :no-load   (set (:no-load opts))
     :files     files
     :since     now}))

(defn init
  ([opts] (init *state opts))
  ([*state opts]
   (reset! *state (init-impl opts))))

(defn- scan-impl [state]
  (let [{:keys [dirs since load unload no-unload no-load files]} state
        now           (now)
        changed-files (changed-files dirs since)
        since'        (reduce (fn [n [_ {:keys [modified]}]]
                                (max n modified))
                              now changed-files)
        changed-nses  (mapcat (comp keys :namespaces val)
                              changed-files)
        loaded        @@#'clojure.core/*loaded-libs*
        
        unload?       #(and
                         (loaded %)
                         (not (no-unload %))
                         (not (no-load %)))
        dependees     (dependees files)
        topo-sort     (topo-sort-fn dependees)
        unload'       (->> changed-nses
                        (filter unload?)
                        (transitive-closure dependees)
                        (filter unload?)
                        (concat unload)
                        (topo-sort)
                        (reverse))
        
        load?         #(and
                         (loaded %)
                         (not (no-load %)))
        files'        (into files changed-files)
        dependees'    (clj-reload.core/dependees files')
        topo-sort'    (topo-sort-fn dependees')
        load'         (->> changed-nses
                        (filter load?)
                        (transitive-closure dependees')
                        (filter load?)
                        (concat load)
                        (topo-sort'))]
    (assoc state
      :since  since'
      :unload unload'
      :load   load'
      :files  files')))

(defn ns-unload [ns]
  (try
    (remove-ns ns)
    (dosync
      (alter @#'clojure.core/*loaded-libs* disj ns))
    (when *log-fn*
      (*log-fn* :unload ns))
    (catch Throwable t
      (when *log-fn*
        (*log-fn* :unload-fail ns))
      (throw t))))

(defn unload-impl [state]
  (let [{:keys [unload]} state]
    (ns-unload (first unload))
    (assoc state
      :unload (next unload))))

(defn ns-load [ns]
  (try
    (require ns :reload) ;; use load to load?
    (when *log-fn*
      (*log-fn* :load ns))
    (catch Throwable t
      (when *log-fn*
        (*log-fn* :load-fail ns))
      (throw t))))

(defn load-impl [state]
  (let [{:keys [load]} state]
    (ns-load (first load))
    (assoc state
      :load (next load))))

(defn- swap-once! [a state f msg]
  (let [state' (f state)]
    (if (compare-and-set! *state state state')
      state'
      (throw (ex-info (str "*state changed during " msg)
                      {:expected-state state
                       :actual-state @a})))))

(defn reload
  ([] (reload *state))
  ([*state]
   (loop [state (swap-once! *state @state scan-impl "scan-impl")]
     (cond
       (seq (:unload state))
       (recur (swap-once! *state state unload-impl "unload-impl"))

       (seq (:load state))
       (recur (swap-once! *state state load-impl "load-impl"))

       :else
       state))))

(comment
  (dependencies (first-scan ["fixtures"]))
  (dependees (first-scan ["fixtures"]))
  
  (reset! *state (first-scan ["src" "dev" "test"]))
  @*state
  (scan @*state)
  (reload (scan @*state))
  
  (flatten-state @*state)
  (transitive-closure
    (dependencies @*state)
    ['clj-reload.core-test])
  (topo-sort (dependencies @*state)) ;; unload order
  (topo-sort {:a #{:b :c}
              :b #{:c}})
  (dependees @*state))
  
