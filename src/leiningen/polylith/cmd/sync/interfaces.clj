(ns leiningen.polylith.cmd.sync.interfaces
  (:require [clojure.string :as str]
            [leiningen.polylith.file :as file]
            [leiningen.polylith.cmd.shared :as shared]
            [clojure.set :as set]))

(defn ->ifc-components [ws-path top-dir m component all-interfaces]
  (let [interface (shared/interface-of ws-path top-dir component all-interfaces)]
    (if (contains? m interface)
      (assoc m interface (conj (m interface) component))
      (assoc m interface #{component}))))

(defn ->component-path [ws-path top-dir component sub-path]
  (let [ns-path (if (str/blank? top-dir) "" (str "/" top-dir))]
    (str ws-path "/components/" component "/src" ns-path "/" sub-path)))

(defn ->component-paths [ws-path top-dir sub-path ifc->components]
  (let [interface (shared/entity-src-dir-name (first (str/split sub-path #"/")))
        components (sort (ifc->components interface))]
    (map #(->component-path ws-path top-dir % sub-path) components)))

(defn def? [code]
  (if (list? code)
    (let [f (first code)]
      (or (= f 'def) (= f 'defn) (= f 'defmacro) (= f 'malli/defn)))
    false))

(defn read-code [path]
  (filterv def? (drop 1 (file/read-file path))))

(defn mx-defn-args
  [code]
  (loop [arg-list code
         args     []]
    (if (empty? arg-list)
      args
      (if (= (second arg-list) :-)
        (let [[arg _ _ & rest] arg-list]
          (recur rest (conj args arg)))
        (let [[arg & rest] arg-list]
          (recur rest (conj args arg)))))))

(defn def->sig
  [[type name & _]]
  #{{:type  type
     :name  name
     :args  []
     :arity 0}})

(defn mx-defn-code->sig
  [name code]
  (let [args (mx-defn-args (first code))]
    {:type 'defn
     :name name
     :args args
     :arity (count args)}))

(defn mx-defn->sig
  [[_ name & src-code]]
  (let [code (filter #(or (list? %) (vector? %)) src-code)
        code (if (and (vector? (first code))
                      (keyword? (first (first code))))
               (rest code)
               code)]
    (if (vector? (first code))
      #{(mx-defn-code->sig name code)}
      (->> code
           (map (partial mx-defn-code->sig name))
           set))))

(defn other->sig
  [[type name & src-code]]
  (let [code (drop-while #(not (or (list? %)
                                   (vector? %)))
                         src-code)]
    (if (vector? (first code))
      #{{:type  type
         :name  name
         :args  (first code)
         :arity (-> code first count)}}
      (set (map #(hash-map :type type
                           :name name
                           :args (first %)
                           :arity (-> % first count)) code)))))

(defn signatures [src-code]
  "Takes the source code of a def, function or macro
   and returns a list with the signatures."
  (let [type (first src-code)]
    (condp = type
      'def        (def->sig src-code)
      'malli/defn (mx-defn->sig src-code)
      (other->sig src-code))))

(defn ifc-set [defs]
  (set (map :name defs)))

(defn missing-defs [interface-path component-path]
  (let [interface-code (read-code interface-path)
        component-code (read-code component-path)
        interface-defs (set (mapcat signatures interface-code))
        component-defs (set (mapcat signatures component-code))
        missing (set/difference component-defs interface-defs)
        already-defined (set/intersection (ifc-set (filter #(not= 'def (:type %)) missing))
                                          (ifc-set interface-defs))]
    {:missing missing
     :already-defined (filter #(contains? already-defined (:name %)) missing)}))

(defn error-message [{:keys [type name arity]}]
  (cond
    (= type 'defn) (str "\"function '" name "' with arity " arity " must be added manually.\"")
    (= type 'defmacro) (str "\"macro '" name "' with arity " arity " must be added manually.\"")
    :else (str "\"def '" name "' will be added automatically\""))) ;; should never happen!

(defn wspath [ws path]
  (let [index (str/index-of path (str "/" ws "/"))]
    (subs path (+ index (count ws) 2))))

(defn missing-definitions [ws path interface-path component-path]
  (if (not (file/file-exists component-path))
    (let [path (wspath ws component-path)]
      {:ok? false
       :message (str "Expected to find interface '" path "'.")})
    (let [{:keys [missing already-defined]} (missing-defs interface-path component-path)]
      (if (not (empty? already-defined))
        {:ok? false
         :message (str "Workspace interfaces are out of sync in '" path "': "
                       (str/join ", " (map error-message already-defined)))}
        {:ok? true
         :missing missing}))))

(defn def-statement [{:keys [type name args]}]
  (cond
    (= 'def type) (str "(def " name ")")
    :else (str "(" type " " name " " args ")")))

(defn sorting [{:keys [type name arity]}]
  [(str type) (str name) arity])

(defn sync-interface! [ws-path ws top-dir ifc->components sub-path]
  (let [ns-path (if (str/blank? top-dir) "" (str "/" top-dir))
        interface-path (str ws-path "/interfaces/src" ns-path "/" sub-path)
        path (wspath ws interface-path)
        paths (->component-paths ws-path top-dir sub-path ifc->components)
        missing-defs (set (map #(missing-definitions ws path interface-path %) paths))
        errors (str/join ", " (map :message (sort (filter (complement :ok?) missing-defs))))]
    (if (not (empty? errors))
      (do
        (println errors)
        false)
      (let [defs (sort-by sorting (mapcat :missing (filter :ok? missing-defs)))]
        (when (not (empty? defs))
          (println (str "Added these definitions to '" path "':")))
        (doseq [missing-def defs]
          (let [statement (def-statement missing-def)]
            (file/append-to-file interface-path statement)
            (println (str "  " statement))))
        true))))

(defn interface-path [ns-path path]
  (let [index (+ (str/index-of path "/interfaces")
                 (count ns-path)
                 16)]
    (subs path index)))

(defn sync-interfaces! [ws-path top-dir]
  (let [ws (last (str/split ws-path #"/"))
        interfaces (shared/all-interfaces ws-path top-dir)
        components (shared/all-components ws-path)
        ifc->components (reduce #(->ifc-components ws-path top-dir %1 %2 interfaces) {} components)
        ns-path (if (str/blank? top-dir) "" (str "/" top-dir))
        path (str ws-path "/interfaces/src" ns-path)
        interface-paths (mapv str (filterv #(str/ends-with? (str %) ".clj") (file/files path)))
        sub-paths (map #(interface-path ns-path %)
                       interface-paths)
        return-flags (map #(sync-interface! ws-path ws top-dir ifc->components %)
                          (sort sub-paths))]
    (every? true? return-flags)))
