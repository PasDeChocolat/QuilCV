(ns videotest.flock-tune.gesture
  (:require
   [videotest.flock-tune.native-vector :as fvec]
   [videotest.flock-tune.behavior :as beh]
   [videotest.flock-tune.bin-lattice :as binl]))


(def GESTURE-BIN-SIZE binl/VEH-BIN-SIZE)

(defn pt->vec [pt]
  [(.x pt) (.y pt)])

(defn gesture-loc->bin [start-pt]
  (binl/loc->bin (pt->vec start-pt) GESTURE-BIN-SIZE))

(defn pts->gesture [[start-pt end-pt]]
  (let [flow (apply fvec/- (map pt->vec [end-pt start-pt]))]
    [start-pt flow]))

(defn record-gestures [{:keys [flow-pts] :as state}]
  "Records gestures as [start-point flow-vector] pair.
   Input is flow-pts, which is [start-point end-point] pair.
   Flow vector is calculated as distance between these points.
   Gestures are placed in a simple bin lattice hash map."
  (let [gestures (doall
                  (reduce (fn [memo [start-pt flow]]
                            (let [bin (gesture-loc->bin start-pt)]
                              (update-in memo [bin] #(conj (or % []) flow))))
                          {} (map pts->gesture flow-pts)))]
    (-> state
        (assoc-in [:gestures] gestures))))

(defn gestures-near [gestures loc]
  (binl/bin-things-near GESTURE-BIN-SIZE gestures loc))

(defn align-from-gestures
  "Returns the alignment force for nearby gestures,
   an average velocity of sorts. This force must be applied."
  [gestures vehicle]
  (let [{:keys [location max-force max-speed velocity]} vehicle
        gesture-vels (gestures-near gestures location)]
    (beh/align-vec max-force max-speed gesture-vels velocity)))
