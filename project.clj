(defproject jp.ne.tir/tir-js-property "4.0.0"
  :description "js property manipulator utility"
  :url "https://github.com/ayamada/tir-js-property"
  :license {:name "Zlib License"
            :url "https://opensource.org/licenses/Zlib"}
  ;:pedantic? :abort
  :dependencies []
  :plugins [[lein-cljsbuild "1.1.8"]
            [lein-doo "0.1.11" :exclusions [org.clojure/clojure]]]
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.3"]
                                  [org.clojure/clojurescript "1.10.844"]]
                   :test-paths ["test"]}}
  :doo {:build "test"}
  :cljsbuild {:builds {:test {:source-paths ["src" "test"]
                              :incremental? true
                              :compiler {:output-to "target/main.js"
                                         :output-dir "target"
                                         :target :nodejs
                                         :main tir.js.property-test-runner
                                         :optimizations :simple
                                         :pretty-print true
                                         :process-shim false}}}}
  :aliases {"test-cljs" ["doo" "node" "test" "once"]})
