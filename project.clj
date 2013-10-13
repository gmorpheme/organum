(defproject org.clojars.gmorpheme/organum "0.1.2"
  :description "Org-mode parser in clojure"
  :url "https://github.com/gmorpheme/organum"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.typed "0.2.13"]
                 [org.clojure/tools.trace "0.7.6"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}})
