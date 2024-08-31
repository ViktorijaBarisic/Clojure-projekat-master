(defproject clojure-projekat-master "0.1.0-SNAPSHOT"
  :description "Lib for reading data"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.csv "1.1.0"]
                 [incanter "1.9.3"]
                 [org.xerial/sqlite-jdbc "3.42.0.0"]
                 [com.github.seancorfield/next.jdbc "1.2.772"]
                 [nz.ac.waikato.cms.weka/weka-stable "3.8.6"]
                 [midje "1.9.10"]
                 ]
  :main ^:skip-aot clojure-projekat-master.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})




