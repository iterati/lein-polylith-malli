(ns leiningen.polylith.cmd.test-helper
  (:require [clojure.test :refer :all]
            [leiningen.polylith.cmd.info]
            [leiningen.polylith.file :as file]))

(defn settings [ws-dir top-ns]
  {:root ws-dir
   :polylith {:top-namespace top-ns
              :clojure-version "1.9.0"}
   :clojure-version "1.9.0"})

(def root-dir (atom nil))

(defn test-setup-and-tear-down [f]
  (let [path (file/create-temp-dir! "polylith-root")]
    (if path
      (reset! root-dir path)
      (throw (Exception. (str "Could not create directory: " path))))
    (f)
    (file/delete-dir path)))

(use-fixtures :each test-setup-and-tear-down)

(defn content [ws-dir directory]
  (file/read-file (str ws-dir "/" directory)))

(defn interfaces-project-content [name]
  [['defproject name "1.0"
    :description "Component interfaces"
    :dependencies [['org.clojure/clojure "1.9.0"]]
    :aot
    :all]])
