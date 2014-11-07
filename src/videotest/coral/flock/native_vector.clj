;; Based on the Nature of Code
;; by Daniel Shiffman
;; http://natureofcode.com
;;
(ns videotest.coral.flock.native-vector
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.math.numeric-tower :as math]))

(def _+ clojure.core/+)
(def _- clojure.core/-)
(def _* clojure.core/*)
(def _div clojure.core//)


;; Foundation of the type:

(defn- sqr [n]
  (math/expt n 2))

(defn fvec [x y]
  [x y])

(defn x-comp [[x _]]
  x)

(defn y-comp [[_ y]]
  y)



;; Implementation Specific:

(defn euclidean-squared-distance
  "Computes the Euclidean squared distance between two sequences"
  [a b]
  (reduce _+ (map (comp sqr clojure.core/-) a b)))

(defn distance
  "Computes the Euclidean distance between two sequences"
  [a b]
  (math/sqrt (euclidean-squared-distance a b)))

(defn magnitude-squared [v]
  (reduce _+ (map sqr v)))

(defn magnitude [v]
  (math/sqrt (magnitude-squared v)))

(defn x-y [v]
  v)

(defn dot [a b]
  (reduce _+
          (map _* a b)))

(defn normalize [v]
  (let [m (magnitude v)]
    (mapv #(_div % m) v)))


;; Operators:


(defn + [& vs]
  (apply mapv _+ vs))

(defn - [& vs]
  (apply mapv _- vs))

(defn multiply [v scalar]
  (mapv _* (repeat scalar) v))

(defn * [a b]
  (let [[v scalar] (if-not (sequential? a)
                     [b a]
                     [a b])]
    (multiply v scalar)))

(defn / [v scalar]
  (mapv _div v (repeat scalar)))


;; Convenience:

(defn angle-between
  "Computes the angle (in radians) between two vectors."
  [a b]
  (Math/acos (_div (dot a b)
                   (_* (magnitude a)
                       (magnitude b)))))

(defn from-angle [theta]
  (fvec (Math/cos theta) (Math/sin theta)))

(defn limit [v upper]
  (let [m (magnitude v)]
    (if (> m upper)
      (multiply (normalize v) upper)
      v)))

(defn heading [v]
  (let [[x y] (x-y v)]
    (_* -1.0
        (Math/atan2 (_* -1.0
                        y)
                    x))))

(defn scalar-projection
  "Computes the scalar projection of vector p on vector from a to b."
  [p a b]
  (let [ap (- p a)
        ab (- b a)
        ab-unit (normalize ab)
        shadow (* (dot ap ab-unit) ab-unit)]
    (+ a shadow)))

(defn set-mag
  "Create a vector with the magnitude given by len."
  [v len]
  (-> (normalize v)
      (* len)))
