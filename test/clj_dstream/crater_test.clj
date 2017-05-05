(ns clj-dstream.crater-test
  (:require [clojure.test :refer :all]
            [clj-dstream.api :as api]
            [clj-dstream.core :as core]))

(def TWO_PI (* 2 (Math/PI)))

(defn sample-ring [r-inner r-outer]
  (let [r     (+ r-inner
                 (rand (- r-outer r-inner)))
        theta (rand TWO_PI)
        x     (* r (Math/cos theta))
        y     (* r (Math/sin theta))]
    [x y]))

(defn sample-circle [r]
  (sample-ring 0 r))

(defn sample-crater [circle-r ring-minor-r ring-major-r]
  (let [random-thing (rand)]
    (if (>= 0.1 random-thing)
      (sample-circle circle-r)
      (sample-ring ring-minor-r ring-major-r))))

(defn sample-at-time [t time-intervals props]

  (let [phase-space  (::core/phase-space props)
        d1           (first phase-space)
        d2           (second phase-space)
        smallest-dim (min (- (::core/domain-end d1) (::core/domain-start d1))
                          (- (::core/domain-end d2) (::core/domain-start d2)))
        circle-r     (/ smallest-dim 8.0)
        ring-minor-r (+ circle-r (/ smallest-dim 5.0))
        ring-major-r (+ ring-minor-r (/ smallest-dim 10.0))]
    (hash-map
      ::core/raw-datum {::core/position-value
                        (sample-crater circle-r ring-minor-r ring-major-r)}
      ::core/value 1.0)))

(deftest stuff
  (let [props {::core/c_m         3.0
               ::core/c_l         0.8
               ::core/lambda      0.998
               ::core/beta        0.3
               ::core/dimensions  2
               ::core/phase-space [
                                   {::core/domain-start    -1.0
                                    ::core/domain-end      1.0
                                    ::core/domain-interval 0.1}
                                   {::core/domain-start    -1.0
                                    ::core/domain-end      1.0
                                    ::core/domain-interval 0.1}]
               ::core/gap-time    200}]
    (api/iterate-with-sampling sample-at-time 100000 "crater-sampling" props)))