(ns videotest.coral.color
  (:require
   [clojure.math.numeric-tower :as math]))

(defn color-with-alpha [c alpha]
  (conj (vec (take 3 c)) alpha))

(defn color-changed? [current-color previous-color]
  (if (nil? previous-color)
    false
    (let [changes (map (comp math/abs -)
                       current-color previous-color)
          channel-change-big-enough? #(< 50 %)]
      (some channel-change-big-enough? changes))))
