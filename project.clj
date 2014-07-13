(defproject euler-clj "0.1.0-SNAPSHOT"
  :description "Learning Clojure via Project Euler"
  :url "http://projecteuler.net"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]]
  :main ^:skip-aot euler-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
