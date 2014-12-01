(ns videotest.coral.noise
  (:require
   [quil.core :as q]))


(defn xy->perlin [x y]
  (q/noise (* 0.05 x) (* 0.05 y)))

(defn perlin->range [perlin low high]
  (q/map-range perlin 0.0 1.0 low high))
