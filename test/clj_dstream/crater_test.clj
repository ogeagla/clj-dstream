(ns clj-dstream.crater-test
  (:require [clojure.test :refer :all]
            [clj-dstream.api :as api]
            [clj-dstream.core :as core]))

(def PI (Math/PI))
(def PI-OVER-TWO (/ (Math/PI) 2))
(def THREE-PI-OVER-TWO (/ (* 3 (Math/PI)) 2))
(def TWO-PI (* 2 (Math/PI)))

(defn sample-ring [r-inner r-outer]
  ;;     /|
  ;;   r/ |
  ;;   /  | y
  ;;  /t__|
  ;;     x
  ;; sin(t) = y/r
  ;; cos(t) = x/r
  ;;
  ;; y(r, t) = sgny(t) * r * sin(t)
  ;; x(r, t) = sgnx(t) * r * cos(t)
  ;; where:
  ;;  if(t is between 0 and PI/2)
  ;;    sgny(t) = 1
  ;;    sngx(t) = 1
  ;;  if(t is between PI/2 and PI)
  ;;    sngy(t) = 1
  ;;    sngx(t) = -1
  ;;  if(t is beteen PI and 3*PI/2)
  ;;    sngy(t) = -1
  ;;    sngx(t) = -1
  ;;  if(t is between 3*PI/2 and 2*PI)
  ;;    sngy(t) = -1
  ;;    sngx(t) = 1
  ;;
  (let [r     (+ r-inner
                 (rand (- r-outer r-inner)))
        theta (rand TWO-PI)
        [sgnx sgny] (cond
                      (<= 0 theta PI-OVER-TWO) [1.0 1.0]
                      (<= PI-OVER-TWO theta PI) [-1.0 1.0]
                      (<= PI theta THREE-PI-OVER-TWO) [-1.0 -1.0]
                      (<= THREE-PI-OVER-TWO theta TWO-PI) [1.0 -1.0])
        x     (* r (Math/cos theta))
        y     (* r (Math/sin theta))]
    ;(println "ring " r-inner r-outer)
    ;(println "  t,r, x, y, sgnx, sgny" theta r x y sgnx sgny)
    [x y]))

(defn sample-circle [r]
  (sample-ring 0 r))

(defn sample-crater [circle-r ring-minor-r ring-major-r]
  (let [random-thing (rand)]
    (if (<= 0.3 random-thing)
      (sample-circle circle-r)
      (sample-ring ring-minor-r ring-major-r))))

(defn sample-at-time [t time-intervals props]

  (let [phase-space  (::core/phase-space props)
        d1           (first phase-space)
        d2           (second phase-space)
        smallest-dim (min (- (::core/domain-end d1) (::core/domain-start d1))
                          (- (::core/domain-end d2) (::core/domain-start d2)))
        circle-r     (/ smallest-dim 16.0)
        ring-minor-r (+ circle-r (/ smallest-dim 2))
        ring-major-r (+ ring-minor-r (/ smallest-dim 2))]
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
                                    ::core/domain-interval 0.05}
                                   {::core/domain-start    -1.0
                                    ::core/domain-end      1.0
                                    ::core/domain-interval 0.05}]
               ::core/gap-time    200}]
    (api/iterate-with-sampling sample-at-time 20000 "crater-sampling" props)))