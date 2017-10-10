(defproject og/clj-dstream "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [org.clojure/test.check "0.9.0"]
                 [aysylu/loom "1.0.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [thi.ng/geom "0.0.908"]
                 [thinktopic/think.tsne "0.1.1"]
                 [net.mikera/core.matrix "0.61.0"]
                 [com.taoensso/tufte "1.1.2"]
                 [me.raynes/fs "1.4.6"]
                 [slacker "0.15.1"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
