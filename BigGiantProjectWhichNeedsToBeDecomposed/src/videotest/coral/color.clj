(ns videotest.coral.color
  (:require
   [clojure.math.numeric-tower :as math])
  (:import
   [org.opencv.core CvType Mat]
   [org.opencv.imgproc Imgproc]))

(defn color-with-alpha [c alpha]
  (conj (vec (take 3 c)) alpha))

(defn color-changed? [current-color previous-color]
  (if (nil? previous-color)
    false
    (let [changes (map (comp math/abs -)
                       current-color previous-color)
          channel-change-big-enough? #(< 50 %)]
      (some channel-change-big-enough? changes))))

(defn single-three-dim-color-mat []
  (Mat. 1 1 CvType/CV_8UC3))

(defn mat-single-rgb [mat r g b]
  (.put mat 0 0 (double-array [r g b]))
  mat)

(defn rgba->hsva
  ([in-mat out-mat rgba]
     (let [[r g b :as rgb] (take 3 rgba)
           alpha (or (last rgba) 255)]
       (.put in-mat 0 0 (double-array rgb))
       (Imgproc/cvtColor in-mat out-mat Imgproc/COLOR_RGB2HSV)
       (conj
        (vec (.get out-mat 0 0))
        alpha)))
  ([rgba]
     (rgba->hsva (single-three-dim-color-mat)
                 (single-three-dim-color-mat)
                 rgba)))

(defn hue-diff [h1 h2]
  #_(mod (math/abs (- h1 h2))
       127.5)
  (let [diff1 (math/abs (- h1 h2))
        diff2 (math/abs (- (+ 360 h1) h2))
        diff3 (math/abs (- h1 (+ 360 h2)))]
    (min diff1 diff2 diff3)))
