(ns clj-reload.core
  (:require
   [clj-reload.keep :as keep]
   [clj-reload.parse :as parse]
   [clj-reload.plan :as plan]
   [clj-reload.util :as util]
   [clojure.set :as set]
   [clojure.java.io :as io])
  (:import
   [java.util.concurrent.locks ReentrantLock]))

; Config :: {:dirs        [<string> ...]       - where to look for files
;            :files       #"<regex>"           - which files to scan, defaults to #".*\.cljc?"
;            :no-unload   #{<symbol> ...}      - list of nses to skip unload
;            :no-reload   #{<symbol> ...}      - list of nses to skip reload
;            :reload-hook <symbol>             - if function with this name exists,
;                                                it will be called after reloading.
;                                                default: after-ns-reload
;            :unload-hook <symbol>             - if function with this name exists,
;                                                it will be called before unloading.
;                                                default: before-ns-unload
;            :output      <keyword>}           - verbosity of log output. Options:
;                                                :verbose - print Unloading/Reloading for each namespace
;                                                :quieter - only print 'Reloaded N namespaces'
;                                                :quiet - no output at all
;                                                default: :verbose

(def ^:private ^:dynamic *config*)

; State :: {:since       <long>               - last time list of files was scanned
;           :files       {<file> -> File}     - all found files
;           :namespaces  {<symbol> -> NS}     - all found namespaces
;           :to-unload   (<symbol> ...)      - list of nses pending unload
;           :to-load     (<symbol> ...)     - list of nses pending load
;
; File ::  {:namespaces #{<symbol> ...}       - nses defined in this file
;           :modified   <modified>}           - lastModified
;
; NS   ::  {:ns-files    #{<file> ...}        - files containing (ns ...) declaration
;           :in-ns-files #{<file> ...}        - files containing (in-ns ...) declaration
;           :requires    #{<symbol> ...}      - other nses this depends on
;           :meta        {}                   - metadata from ns symbol
;           :keep        {<symbol> -> Keep}}} - vars to keep between reloads
;
; Keep ::  {:tag      <symbol>                - type of value ('def, 'defonce etc)
;           :form     <any>                   - full source form, just in case
;                          
;           // stashed vars                   - one or more of these will contain
;                                               values remembered between reloads
;           :var      Var?
;           :ctor     Var?
;           :map-ctor Var?
;           :proto    Var?
;           :methods  {<symbol> Var}?}

(def ^:private *state
  (atom {}))

(defn with-lock* [id f]
  (let [started (promise)
        acquired (promise)
        cancel (volatile! false)
        fut (future
              (deliver started true)
              (if (Thread/holdsLock clojure.lang.RT/REQUIRE_LOCK)
                (do (deliver acquired :unexpectedly-already-held-by-current-thread)
                    {:result :failed-to-lock})
                (locking clojure.lang.RT/REQUIRE_LOCK
                  (deliver acquired :acquired)
                  (if @cancel
                    {:result :failed-to-lock}
                    (f)))))
        _ @started
        status (deref acquired 1000 :could-not-acquire)]
    (when-not (= :acquired status)
      (vreset! cancel true)
      (future-cancel fut)
      (throw (ex-info "Could not lock" {:id id :status status})))
    (try @fut
         (catch java.util.concurrent.ExecutionException e
           (throw (or (.getCause e) e))))))

(defmacro with-lock [id & body]
  (assert (seq body))
  `(with-lock* ~id #(do ~@body)))

(defn- files->namespaces [files already-read]
  (let [*res (volatile! {})]
    (doseq [file files
            [name namespace] (or
                               (already-read file)
                               (parse/read-file file))
            :when (not (util/throwable? namespace))]
      (vswap! *res update name #(merge-with into % namespace)))
    @*res))

(defn- scan-impl [{files-before :files
                   nses-before  :namespaces} since config]
  (let [files-now        (->> (:dirs config)
                           (mapcat #(file-seq (io/file %)))
                           (filter util/file?)
                           (filter #(re-matches (:files config) (util/file-name %))))

        [files-modified
         files-broken]   (reduce
                           (fn [[modified broken] file]
                             (if (<= (util/last-modified file) since)
                               [modified broken]
                               (let [res (parse/read-file file)]
                                 (if (util/throwable? res)
                                   [modified (assoc broken file res)]
                                   [(assoc modified file res) broken]))))
                           [{} {}] files-now)

        files-deleted    (reduce disj (set (keys files-before)) files-now)
        
        nses-broken      (util/for-map [[file ex] files-broken
                                        ns (get-in files-before [file :namespaces])]
                           [ns ex])
        
        nses-unload      (reduce
                           #(into %1 (get-in files-before [%2 :namespaces]))
                           #{}
                           (concat (keys files-modified) files-deleted))
        
        nses-load        (util/for-set [[file namespaces] files-modified
                                        ns (keys namespaces)]
                           ns)
        
        files'           (as-> files-before %
                           (reduce dissoc % files-deleted)
                           (merge %
                             (util/for-map [[file namespaces] files-modified]
                               [file {:namespaces (set (keys namespaces))
                                      :modified   (util/last-modified file)}])))
        
        already-read     (merge
                           (util/for-map [[file {:keys [namespaces]}] files-before]
                             [file (select-keys nses-before namespaces)])
                           files-modified
                           (util/for-map [[file _] files-broken]
                             [file {}]))
        
        nses'            (files->namespaces (keys files') already-read)]
                           
    {:broken      nses-broken
     :files'      files'
     :namespaces' nses'
     :to-unload'  nses-unload
     :to-load'    nses-load}))

(defn find-namespaces
  "Returns namespaces matching regex, or all of them"
  ([]
   (find-namespaces #".*"))
  ([regex]
   (binding [util/*log-fn* nil]
     (let [{:keys [namespaces']} (scan-impl @*state 0 *config*)]
       (into #{} (filter #(re-matches regex (name %)) (keys namespaces')))))))

(def ^{:doc "Returns dirs that are currently on classpath"
       :arglists '([])}
  classpath-dirs
  util/classpath-dirs)

(defn init
  "Options:
   
   :dirs        :: [<string> ...]  - where to look for files
   :files       :: #\"<regex>\"    - which files to scan, defaults to #\".*\\\\.cljc?\"
   :no-reload   :: #{<symbol> ...} - list of namespaces to skip reload entirely
   :no-unload   :: #{<symbol> ...} - list of namespaces to skip unload only.
                                     These will be loaded “on top” of previous state
   :unload-hook :: <symbol>        - if function with this name exists in a namespace,
                                     it will be called before unloading. Default: 'before-ns-unload
   :reload-hook :: <symbol>        - if function with this name exists in a namespace,
                                     it will be called after reloading. Default: 'after-ns-reload
   :output      :: <keyword>       - verbosity of log output. Options:
                                     :verbose - print Unloading/Reloading for each namespace
                                     :quieter - only print 'Reloaded N namespaces'
                                     :quiet - no output at all
                                     Default: :verbose"
  [opts]
  (with-lock ::init
    (binding [util/*log-fn* nil]
      (let [dirs  (vec (or (:dirs opts) (classpath-dirs)))
            files (or (:files opts) #".*\.cljc?")
            now   (util/now)
            config {:dirs        dirs
                    :files       files
                    :no-unload   (set (:no-unload opts))
                    :no-reload   (set (:no-reload opts))
                    :reload-hook (:reload-hook opts 'after-ns-reload)
                    :unload-hook (:unload-hook opts 'before-ns-unload)
                    :output      (:output opts :verbose)}]
        (alter-var-root #'*config* (constantly config))
        (let [{:keys [files' namespaces']} (scan-impl nil 0 config)]
          (reset! *state {:since       now
                          :files       files'
                          :namespaces  namespaces'}))))))

(defn- add-unloaded [scan re loaded]
  (let [new (->> (keys (:namespaces' scan))
              (remove loaded)
              (filter #(re-matches re (str %))))]
    (update scan :to-load' into new)))

(defn carry-keeps [from to]
  (util/for-map [[ns-sym ns] to]
    [ns-sym (assoc ns :keep
              (merge-with merge
                (get-in from [ns-sym :keep])
                (:keep ns)))]))

(defn- scan [state config opts]
  (assert (not (thread-bound? #'clojure.core/*loaded-libs*)))
  (let [{:keys [no-unload no-reload]} config
        {:keys [since to-load to-unload files namespaces]} state
        {:keys [only] :or {only :changed}} opts
        now              (util/now)
        loaded           @@#'clojure.core/*loaded-libs*
        {:keys [broken
                files'
                namespaces'
                to-unload'
                to-load']} (case only
                             :changed (scan-impl state since config)
                             :loaded  (scan-impl state 0 config)
                             :all     (scan-impl state 0 config)
                             #_regex  (-> (scan-impl state since config)
                                        (add-unloaded only loaded)))
        
        _                (doseq [[ns {:keys [exception]}] broken
                                 :when (loaded ns)
                                 :when (not (no-reload ns))]
                           (throw exception))
        
        since'           (transduce (map :modified) max (max since now) (vals files'))
        
        unload?          #(and
                            (loaded %)
                            (not (:clj-reload/no-unload (:meta (namespaces %))))
                            (not (:clj-reload/no-reload (:meta (namespaces %))))
                            (not (no-unload %))
                            (not (no-reload %)))
        deps             (parse/dependees namespaces)
        topo-sort        (plan/topo-sort-fn deps)
        to-unload''      (->> to-unload'
                           (filter unload?)
                           (parse/transitive-closure deps)
                           (filter unload?)
                           (concat to-unload)
                           (topo-sort)
                           (reverse))
        
        load?            #(and
                            (case only
                              :changed (loaded %)
                              :loaded  (loaded %)
                              :all     true
                              #_regex  (or (loaded %) (re-matches only (str %))))
                            (not (:clj-reload/no-reload (:meta (namespaces %))))
                            (not (no-reload %))
                            (namespaces' %))
        deps'            (parse/dependees namespaces')
        topo-sort'       (plan/topo-sort-fn deps')
        to-load''        (->> to-load'
                           (filter load?)
                           (parse/transitive-closure deps')
                           (filter load?)
                           (concat to-load)
                           (topo-sort'))]
    (assoc state
      :since      since'
      :files      files'
      :namespaces (carry-keeps namespaces namespaces')
      :to-unload  to-unload''
      :unloaded-volatiles (zipmap to-unload'' (repeatedly #(volatile! nil)))
      :to-load    to-load''
      :loaded-volatiles (zipmap to-load'' (repeatedly #(volatile! nil)))
      ::unloaded []
      ::loaded [])))

(defn- ns-unload [ns config opts]
  (when (= (:output config) :verbose)
    (util/log "Unloading" ns))
  (try
    (when-some [unload-hook (:unload-hook config)]
      (when-some [ns-obj (find-ns ns)]
        (when-some [unload-fn (ns-resolve ns-obj unload-hook)]
          (unload-fn))))
    (catch Throwable t
      ;; eat up unload error
      ;; if we can’t unload there’s no way to fix that
      ;; because any changes would require reload first
      (util/log "  exception during unload hook" t)))
  (remove-ns ns)
  (assert (not (thread-bound? #'clojure.core/*loaded-libs*)))
  (dosync
    (commute @#'clojure.core/*loaded-libs* disj ns)))

(defn- ns-load [ns file keeps config opts]
  (when (= (:output config) :verbose)
    (util/log "Loading" ns #_"from" #_(util/file-path file)))
  (try
    (if (empty? keeps)
      (util/ns-load-file (slurp file) ns file)
      (keep/ns-load-patched ns file keeps))
    
    (when-some [reload-hook (:reload-hook config)]
      (when-some [reload-fn (ns-resolve (find-ns ns) reload-hook)]
        (reload-fn)))
    
    nil
    (catch Throwable t
      (util/log "  failed to load" ns t)
      t)))

(defn- do-unload
  "Returns nil or error map."
  [ns state config opts]
  (let [keeps (keep/resolve-keeps ns (-> state :namespaces ns :keep))]
    (ns-unload ns config opts)
    (swap! *state
           (fn [state]
             (-> state
                 (update ::unloaded conj ns)
                 (update :to-unload (fn [to-unload] (doall (remove #(= ns %) to-unload))))
                 (update :namespaces update ns update :keep util/deep-merge keeps))))
    nil))

(defn- do-load
  "Returns nil or error map."
  [ns {::keys [loaded unloaded] :as state} config opts]
  (let [files (-> state :namespaces ns :ns-files)]
    (if-some [ex (some #(ns-load ns % (-> state :namespaces ns :keep) config opts) files)]
      (do
        (swap! *state update :to-unload #(cons ns %))
        (if (:throw opts true)
          (throw
            (ex-info
              (str "Failed to load namespace: " ns)
              {:unloaded unloaded
               :loaded   loaded
               :failed   ns}
              ex))
          {:unloaded  unloaded
           :loaded    loaded
           :failed    ns
           :exception ex}))
      (do
        (swap! *state #(-> %
                           (update ::loaded conj ns)
                           (update :to-load (fn [to-load] (doall (remove #{ns} to-load))))
                           (update-in [:namespaces ns] dissoc :keep)))
        nil))))

(declare fj-reload1)

(defn- fj-fork [forks config {::keys [^java.util.concurrent.ExecutorService threadpool cancel] :as opts}]
  (some (fn [^java.util.concurrent.Future future]
          (try (.get future)
               (catch java.util.concurrent.ExecutionException e
                 (vreset! cancel true)
                 (throw (or (.getCause e) e)))))
        (.invokeAll threadpool
                    (mapv (fn [fork]
                            (bound-fn [] (fj-reload1 fork config opts)))
                         forks))))

(defn- do-task
  "Returns nil on success, or error map (or throws) on error."
  [{:keys [op ns]} config {::keys [cancel skip] :as opts}]
  (if @cancel
    {:result :cancelled :ns ns}
    (case op
      :unload (when-not (contains? skip :unload)
                (do-unload ns @*state config opts))
      :load (when-not (contains? skip :load)
              (do-load ns @*state config opts)))))

(defn- do-synchronous-tasks
  "Returns nil on success, or error map (or throws) on error."
  [tasks config opts]
  (some #(do-task % config opts) tasks))

(defn- fj-reload1
  "Returns nil on success, or error map (or throws) on error."
  [{:keys [before forks after]} config opts]
  (or (do-synchronous-tasks before config opts)
      (fj-fork forks config opts)
      (do-synchronous-tasks after config opts)))

;;TODO fork => unload, join => load
;#_
(defn- fj-reload
  "Options:

  :throw  :: true | false  - throw or return exception, default true
  :log-fn :: (fn [& args]) - fn to display unload/reload status
  :only   :: :changed      - default. Only reloads changed already loaded files
  | :loaded       - Reload all loaded files
  | <Pattern>     - Reload all nses matching this pattern
  | :all          - Reload everything it can find in dirs

  Returns map of what was reloaded

  {:unloaded [<symbol> ...]
   :loaded   [<symbol> ...]}

  If anything fails, throws. If :throw false, return value will also have keys

  {:failed    <symbol>
   :exception <Throwable>}

  Can be called multiple times. If reload fails, fix the error and call `fj-reload` again"
  [opts]
  (with-lock ::fj-reload
    (binding [util/*log-fn* (:log-fn opts util/*log-fn*)]
      (let [t (System/currentTimeMillis)
            config *config*
            state (swap! *state scan config opts)
            cancel (volatile! false)
            max-parallelism 1
            threadpool (java.util.concurrent.Executors/newWorkStealingPool 1)
            opts (assoc opts ::cancel cancel ::threadpool threadpool)
            plan (plan/fj-plan state)]
        (when (= (:output config) :quieter)
          (util/log (format "Reloading %s namespaces..." (count (:to-load state)))))
        (try (or (when-some [err (fj-fork plan config opts)]
                   (vreset! cancel true)
                   err)
                 (-> @*state
                     (select-keys [::unloaded ::loaded])
                     (set/rename-keys {::unloaded :unloaded ::loaded :loaded})))
             (finally
               (.shutdown threadpool)))))))

(defn unload
  "Same as `reload`, but does not load namespaces back"
  ([]
   (unload nil))
  ([opts] (fj-reload (assoc opts ::skip #{:load}))))

(defn reload
  "Options:
   
     :throw  :: true | false  - throw or return exception, default true
     :log-fn :: (fn [& args]) - fn to display unload/reload status
     :only   :: :changed      - default. Only reloads changed already loaded files
              | :loaded       - Reload all loaded files
              | <Pattern>     - Reload all nses matching this pattern
              | :all          - Reload everything it can find in dirs
   
   Returns map of what was reloaded
   
     {:unloaded [<symbol> ...]
      :loaded   [<symbol> ...]}
   
   If anything fails, throws. If :throw false, return value will also have keys
   
     {:failed    <symbol>
      :exception <Throwable>}
   
   Can be called multiple times. If reload fails, fix the error and call `reload` again"
  ([]
   (reload nil))
  ([opts]
   (fj-reload opts)))

(defmulti keep-methods
  (fn [tag]
    tag))

(defmethod keep-methods :default [tag]
  (throw
    (ex-info
      (str "Keeping " tag " forms is not implemented")
      {:tag tag})))
    
(defmethod keep-methods 'def [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'defn [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'defn- [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'defonce [_]
  keep/keep-methods-defs)

(defmethod keep-methods 'deftype [_]
  keep/keep-methods-deftype)

(defmethod keep-methods 'defrecord [_]
  keep/keep-methods-defrecord)

(defmethod keep-methods 'defprotocol [_]
  keep/keep-methods-defprotocol)

;; Initialize with classpath-dirs to support “init-less” workflow
;; See https://github.com/tonsky/clj-reload/pull/4
;; and https://github.com/clojure-emacs/cider-nrepl/issues/849
(when (and
        (not= "false" (System/getenv "CLJ_RELOAD_AUTO_INIT"))
        (not= "false" (System/getProperty "clj-reload.auto-init")))
  (init
    {:dirs (classpath-dirs)}))
