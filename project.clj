(defproject acyclic/utils  "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.7.0"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [com.taoensso/timbre "3.2.0"] 
                 [org.clojure/tools.reader "0.8.5"]
                 [org.clojure/tools.cli "0.3.1"]]

  :aot [acyclic.utils.cli])
