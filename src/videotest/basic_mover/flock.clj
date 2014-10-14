(ns videotest.basic-mover.flock
  (:require
   [quil.core :as q]
   [videotest.basic-mover.behavior :as beh]
   [videotest.basic-mover.native-vector :as fvec]))

(def VEHICLE-R 1.0)

(def SEPARATION-DIST 30)
(def NEIGHBOR-DIST 100)
(def GLOM-DIST 50)

(defn random-v-comp []
  (let [r (+ 20.0 (rand 40.0))]
    (if (> (rand 1.0) 0.5)
      (* -1 r)
      r)))

(defn random-vehicle
  ([{:keys [width height]}]
     (random-vehicle (rand width) (rand height)))
  ([x y]
     {:location (fvec/fvec x y)
      :velocity (fvec/fvec (random-v-comp)
                           (random-v-comp))
      :acceleration (fvec/fvec 0.0 0.0)
      :max-speed 3.0
      :max-force 0.2}))

(defn init-vehicles [width height n]
  (vec
   (take n (repeatedly #(random-vehicle width height)))))

(defn flock [all vehicle]
  (let [sep-factor   1.5
        align-factor 1.0
        glom-factor  1.0
        sep-force (fvec/*
                   (beh/separate SEPARATION-DIST all vehicle)
                   sep-factor)
        align-force (fvec/*
                     (beh/align NEIGHBOR-DIST all vehicle)
                     align-factor)
        glom-force (fvec/*
                    (beh/glom GLOM-DIST all vehicle)
                    glom-factor)]
    (-> vehicle
        (beh/apply-force sep-force)
        (beh/apply-force align-force)
        (beh/apply-force glom-force))))

(defn update-vehicles [width height vehicles]
  (doall
   (map #(->> %
              (flock vehicles)
              (beh/move-vehicle)
              (beh/borders width height VEHICLE-R))
        vehicles)))

(defn draw-vehicle
  [{:keys [location velocity]}]
  (let [[x y] (fvec/x-y location)
        theta (+ (/ Math/PI 2.0)
                 (fvec/heading velocity))]
    (q/with-translation [x y]
      (q/with-rotation [theta]
        (q/begin-shape)
        (q/vertex 0                  (* -2.0 VEHICLE-R))
        (q/vertex (* -1.0 VEHICLE-R) (* 2.0 VEHICLE-R))
        (q/vertex VEHICLE-R          (* 2.0 VEHICLE-R))
        (q/end-shape :close)))))
