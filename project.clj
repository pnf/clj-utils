(defproject acyclic/utils  "0.0.2"
  :description "Simple clojure utilities"
  :url "http://github.com/pnf/clj-utils"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.typed "0.2.72"]
                 [prismatic/schema "0.3.3"]
                 [clj-time "0.7.0"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [com.taoensso/timbre "3.3.1"
                  :exclusions
                  [com.taoensso/encore]] ;; to pick up later version for carmine
                 [com.taoensso/carmine "2.7.0"
                  :exclusions
                  [org.clojure/clojure org.clojure/tools.reader org.clojure/clojurescript]]
                 [com.draines/postal "1.11.1"] ;so timbre/carmine works 
                 [org.clojure/tools.reader "0.8.8"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/algo.monads "0.1.5"]
                 ]

  :aot [acyclic.utils.cli]


  :deploy-repositories [["clojars" :clojars]
                        ;["clojars" {:url https://clojars.org/repo :creds :gpg}]
                        ]

  :scm {:name git
        :url "https://github.com/pnf/clj-utils"}

  :profiles {:dev {:dependencies [[jonase/eastwood "0.2.0" :exclusions [org.clojure/clojure]]]}}




)
