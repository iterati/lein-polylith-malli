(defproject polylith/lein-polylith-malli "0.2.5-SNAPSHOT"
  :description "Polylith - a component based architecture, by Joakim Tengstrand. Forked to add support for malli.experimental/defn."
  :url "https://github.com/iterati/lein-polylith-malli"
  :license {:name "Eclipse Public License",
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git", :url "https://github.com/iterati/lein-polylith-malli"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [zprint "0.4.15"]
                 [org.freemarker/freemarker "2.3.28"]
                 [metosin/malli "0.14.0"]]

  :eval-in-leiningen true)
