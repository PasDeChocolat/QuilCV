(ns videotest.coral.flocking-seeds
  (:require
   [videotest.coral.flock.native-vector :as fvec]))

(defn init-flocking-seed
  [[x y] [vx vy] rgba hsva]
  {:location (fvec/fvec x y)
   :velocity (fvec/fvec vx vy)
   :acceleration (fvec/fvec 0.0 0.0)
   :max-speed 3.0
   :max-force 0.2
   :color-rgba rgba
   :color-hsva hsva})

(defn vel [{:keys [velocity]}]
  (fvec/x-y velocity))

(defn loc [{:keys [location]}]
  (fvec/x-y location))

(defn update-loc [seed x y]
  (assoc-in seed [:location] (fvec/fvec x y)))

(defn update-vel [seed vx vy]
  (assoc-in seed [:velocity] (fvec/fvec vx vy)))
