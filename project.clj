(defproject evolutionary-computation-benchmark-functions "0.2.2"
  :description "A Clojure library designed to run benchmark functions which are famous in evolutionary computation groups.
  The library status is under development."
  :url "https://github.com/Miyoshi-Ryota/evolutionary-computation-benchmark-functions-clojure"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :repositories [["clojars" {:url "https://clojars.org/repo" :sign-releases false}]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :repl-options {:init-ns evolutionary-computation-benchmark-functions.core})
