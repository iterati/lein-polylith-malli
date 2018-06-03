(ns leiningen.polylith.cmd.success-test
  (:require [clojure.test :refer :all]
            [leiningen.polylith.cmd.test-helper :as helper]
            [leiningen.polylith.file :as file]
            [leiningen.polylith :as polylith]
            [leiningen.polylith.cmd.shared :as shared]))

(use-fixtures :each helper/test-setup-and-tear-down)

(defn fake-fn [& args]
  args)

(deftest polylith-success--local--print-to-time
  (with-redefs [file/current-path                (fn [] @helper/root-dir)
                leiningen.polylith.cmd.shared/sh fake-fn]
    (let [ws-dir  (str @helper/root-dir "/ws1")
          project (helper/settings ws-dir "my.company")
          _       (polylith/polylith nil "create" "w" "ws1" "my.company" "-git")
          _       (polylith/polylith project "create" "c" "comp1")
          _       (polylith/polylith project "create" "s" "system1" "base1")
          _       (polylith/polylith project "success")]

      (is (< 0 (-> (helper/content ws-dir ".polylith/time.edn")
                   first :last-successful-build))))))

(deftest polylith-success--ci--print-to-git
  (with-redefs [file/current-path                (fn [] @helper/root-dir)]
    (let [_       (System/setProperty "CI" "CIRCLE")
          ws-dir  (str @helper/root-dir "/ws1")
          project (helper/settings ws-dir "my.company")
          _       (polylith/polylith nil "create" "w" "ws1" "my.company" "-git")
          _       (polylith/polylith project "create" "c" "comp1")
          _       (polylith/polylith project "create" "s" "system1" "base1")
          _       (shared/sh "git" "init" :dir ws-dir)
          _       (shared/sh "git" "add" "." :dir ws-dir)
          _       (shared/sh "git" "commit" "-m" "Initial Commit" :dir ws-dir)
          _       (polylith/polylith project "success")
          _       (System/clearProperty "CI")]

      (is (not (nil? (-> (helper/content ws-dir ".polylith/git.edn")
                       first :last-successful-build)))))))
