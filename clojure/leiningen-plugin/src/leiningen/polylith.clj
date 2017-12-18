(ns leiningen.polylith
  (:require [leiningen.polylith.cmd :as cmd]
            [leiningen.polylith.file :as file]))

(defn polylith
  "Helps you write component based systems"
  ([project]
   (cmd/help []))
  ([project subtask & args]
   (let [settings (:polylith project)
         root-dir (:root-dir settings (file/parent-path))
         ignore-tests (:ignore-tests settings [])
         top-ns (:top-ns settings)
         top-dir (:top-dir settings "")
         dev-dirs (:development-dirs settings ["development"])]
     (case subtask
       "changes" (cmd/changes root-dir args)
       "delete" (cmd/delete root-dir dev-dirs args)
       "deps" (cmd/deps root-dir)
       "diff" (cmd/diff root-dir args)
       "help" (cmd/help args)
       "info" (cmd/info root-dir args)
       "new" (cmd/new-cmd root-dir top-ns dev-dirs args)
       "settings" (cmd/settings root-dir settings)
       "tests" (cmd/tests root-dir ignore-tests args)
       (cmd/task-not-found subtask)))))
