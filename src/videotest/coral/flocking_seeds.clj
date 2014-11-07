(ns videotest.coral.flocking-seeds
  (:require
   [quil.core :as q]
   [videotest.coral.flock.behavior :as beh]
   [videotest.coral.flock.flock :as flock]
   [videotest.coral.flock.native-vector :as fvec]))

(def MAX-SPEED 8.0)
(def MAX-FORCE 0.9)
(def FLOCK-MOD-CYCLE 2)

(defn init-flocking-seed
  [[x y] [vx vy] rgba hsva]
  {:location (fvec/fvec x y)
   :velocity (fvec/fvec vx vy)
   :acceleration (fvec/fvec 0.0 0.0)
   :max-speed MAX-SPEED
   :max-force MAX-FORCE
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

(defn record-flock [{:keys [motion-seeds] :as state}]
  (let [new-locs (doall
                  (reduce (fn [memo seed]
                            (let [bin (flock/veh-loc->bin (:location seed))]
                              (update-in memo [bin] #(conj (or % []) seed))))
                          {} motion-seeds))]
    (-> state
        (assoc-in [:seed-locations] new-locs))))

(defn flock-move [width height seed-locations seeds]
  (let [flock-near (fn [v]
                     (if (= 0 (mod (q/frame-count) FLOCK-MOD-CYCLE))
                       (flock/flock (flock/vehicles-near seed-locations v)
                                   v)
                       v))]
    (doall
     (mapv #(->> %
                 (flock-near)
                 (beh/move-vehicle)
                 #_(beh/borders width height VEHICLE-R))
           seeds))))

(defn update-vehicles [state width height]
  (let [{:keys [seed-locations]} state]
    (-> state
        (update-in [:motion-seeds] (partial flock-move width height seed-locations))
        (record-flock))))
