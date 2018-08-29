(ns leiningen.polylith.cmd.help.test
  (:require [leiningen.polylith.cmd.shared :as shared]))

(defn help []
  (println "  Executes component and base tests.")
  (println)
  (println "  The following steps are performed:")
  (println "    - checks for circular dependencies and stops if found.")
  (println "    - calculates what components and bases to process based on what has")
  (println "      changed since the last successful build.")
  (println "    - calls 'sync' and makes sure that all dependencies in project.clj")
  (println "      files are in sync.")
  (println "    - AOT compile changed components, bases and systems to check that they compile")
  (println "      and fulfill public interfaces.")
  (println "    - runs tests for all bases and components that have been affected by the changes.")
  (println "    - if the entire build is successful and +success is set, then execute the success")
  (println "      command that updates the time for the last successful build.")
  (println)
  (println "  lein polylith test [ARG] [SKIP]")
  (println "    ARG = (omitted) -> Since last successful build, stored in bookmark")
  (println "                       :last-successful-build in WS-ROOT/.polylith/time.edn")
  (println "                       or :last-successful-build in WS-ROOT/.polylith/git.edn")
  (println "                       if you have the CI variable set to something on the machine.")
  (println "          timestamp -> Since the given timestamp (milliseconds since 1970).")
  (println "          git-hash  -> Since the given git hash if the CI variable is set.")
  (println "          bookmark  -> Since the timestamp for the given bookmark in")
  (println "                       WS-ROOT/.polylith/time.edn or since the git hash")
  (println "                       for the given bookmark in WS-ROOT/.polylith/git.edn")
  (println "                       if CI variable set.")
  (println)
  (println "    SKIP = (omitted)      -> Executes all steps.")
  (println "           -circular-deps -> Skips checking for circular dependencies step.")
  (println "           -sync          -> Skips dependency sync step.")
  (println "           -compile       -> Skips compilation step.")
  (println "           +success       -> Saves time/git-sha1 after running tests. If a bookmark")
  (println "                             is not provided, last-successful-test will be used. By")
  (println "                             default, test command still uses last-successful-build")
  (println "                             for testing. If you want to use last-successful-test,")
  (println "                             provide last-successful-test as a bookmark argument.")
  (println)
  (println "  'lein polylith test 0' can be used to test all files in the workspace")
  (println "  (or at least changes since 1970-01-01).")
  (println)
  (println "  examples:")
  (println "    lein polylith test")
  (println "    lein polylith test -compile")
  (if (shared/ci?)
    (println "    lein polylith test 7d7fd132412aad0f8d3019edfccd1e9d92a5a8ae")
    (println "    lein polylith test 1523649477000"))
  (println "    lein polylith test mybookmark")
  (println "    lein polylith test mybookmark -compile")
  (println "    lein polylith test last-successful-test -compile +success"))
