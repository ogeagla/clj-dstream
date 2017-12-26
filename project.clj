(defproject og/clj-dstream "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-ancient "0.6.14"]
            [jonase/eastwood "0.2.5"]
            [lein-kibit "0.1.5"]
            [lein-bikeshed "0.5.0"]
            [venantius/ultra "0.5.2"]
            [lein-figwheel "0.5.13"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/test.check "0.9.0"]
                 [aysylu/loom "1.0.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [thi.ng/geom "0.0.1062"]
                 [thinktopic/think.tsne "0.1.1"]
                 [net.mikera/core.matrix "0.61.0"]
                 [com.taoensso/tufte "1.1.2"]
                 [me.raynes/fs "1.4.6"]
                 [slacker "0.16.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
