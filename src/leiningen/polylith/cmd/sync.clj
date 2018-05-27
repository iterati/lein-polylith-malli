(ns leiningen.polylith.cmd.sync
  (:require [leiningen.polylith.file :as file]
            [clojure.set :as set]
            [leiningen.polylith.cmd.shared :as shared]))

(defn libraries [path]
  (let [content (first (file/read-file path))
        index (ffirst
                (filter #(= :dependencies (second %))
                        (map-indexed vector content)))]
    (nth content (inc index))))

(defn deps-index [content]
  (ffirst
    (filter #(= :dependencies (second %))
            (map-indexed vector content))))

(defn merge-libs [dev-libs project-path]
  (let [comp1-libs (libraries project-path)
        dev-key (fn [m [k v]]
                  (if (contains? dev-libs k)
                    (into m [[k (dev-libs k)]])
                    (into m [[k v]])))]
    (reduce dev-key {} comp1-libs)))

(defn dep-row [i [[lib version] n]]
  (str "                 [" lib
       " \"" version "\"]"
       (if (= i n) "]" "")))

(defn align-libs [[k v]]
  (if (= :dependencies k)
    (let [n (-> v count dec dec)]
      (cons (str "  " k " [" (first v))
            (map-indexed dep-row (map #(vector % n) (rest v)))))
    (if (string? v)
      [(str "  " k " \"" v "\"")]
      [(str "  " k " " v)])))

(defn lib-versions-has-changed? [dev-project-path project-path]
  (let [dev-libs (into {} (libraries dev-project-path))
        libs (into {} (libraries project-path))
        shared-libs (set/intersection (set (map first libs))
                                      (set (map first dev-libs)))]
    (not
      (empty?
        (set/difference
          (set (filter #(contains? shared-libs (first %)) dev-libs))
          (set (filter #(contains? shared-libs (first %)) libs)))))))

(defn updated-content [dev-project-path project-path]
  (let [dev-libs (into {} (libraries dev-project-path))
        updated-libs (vec (map identity (merge-libs dev-libs project-path)))
        content (vec (first (file/read-file project-path)))
        index (inc (deps-index content))]
    (assoc content index updated-libs)))

(defn update-libraries! [content target-path]
  (let [[a b c & kv-pairs] content
        formatted-content (doall (cons (str "(" a " " b " \"" c "\"")
                                       (mapcat align-libs (partition 2 kv-pairs))))]
    (when (not (empty? formatted-content))
      (spit target-path (str (first formatted-content) "\n"))
      (doseq [row (drop-last (rest formatted-content))]
        (spit target-path (str row "\n") :append true))
      (spit target-path (str (last formatted-content) ")\n") :append true))))

(defn sync-entities! [ws-path dev-project-path entities-name entities]
  (doseq [entity entities]
    (let [target-path (str ws-path "/" entities-name "/" entity "/project.clj")]
      (when (lib-versions-has-changed? dev-project-path target-path)
        (update-libraries! (updated-content dev-project-path target-path)
                           target-path)))))

(defn entity-libs [ws-path type entities]
  (into (sorted-map)
        (filter #(not= "interfaces" (-> % first name))
                (mapcat #(libraries (str ws-path "/" type "/" % "/project.clj"))
                        entities))))

(defn updated-system-content [libs project-path]
  (let [content (vec (first (file/read-file project-path)))
        index (inc (deps-index content))]
    (assoc content index libs)))

(defn update-systems! [ws-path top-dir]
  (let [bases (shared/all-bases ws-path)
        components (shared/all-components ws-path)]
    (doseq [system (shared/all-systems ws-path)]
      (let [system-path (str ws-path "/systems/" system)
            project-path (str system-path "/project.clj")
            src-path (str system-path "/src/" top-dir)
            entities (file/directory-names src-path)
            base-libs (entity-libs ws-path "bases"
                                   (filter #(contains? bases %) entities))
            comp-libs (entity-libs ws-path "components"
                                   (filter #(contains? components %) entities))
            new-libs (sort (set (concat base-libs comp-libs)))
            sys-libs (sort (libraries project-path))
            content (updated-system-content new-libs project-path)]
        (when (not= new-libs sys-libs)
          (update-libraries! content project-path))))))

(defn execute [ws-path top-dir]
  (let [dev-project-path (str ws-path "/environments/development/project.clj")]
    (sync-entities! ws-path dev-project-path "components" (shared/all-components ws-path))
    (sync-entities! ws-path dev-project-path "bases" (shared/all-bases ws-path))
    (update-systems! ws-path top-dir)))
