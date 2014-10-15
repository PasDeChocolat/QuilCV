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
   (take n (repeatedly #(random-vehicle {:width width :height height})))))

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

(defn vehicles-near [vehicle-locations vehicle]
  (let [[x y] (fvec/x-y (:location vehicle))
        col (int (/ x BIN-SIZE))
        row (int (/ y BIN-SIZE))
        bins (for [i (range -1 2)
                   j (range -1 2)
                   :let [bin-col (+ col i)
                         bin-row (+ row j)]]
               [bin-col bin-row])]
    (vec
     (doall
      (reduce (fn [memo bin]
                (concat memo (get vehicle-locations bin [])))
              [] bins)))))

(defn flock-move [width height vehicle-locations vehicles]
  (let [flock-near #(fn [v]
                      (flock (vehicles-near vehicle-locations v)
                             v))]
    (doall
     (mapv #(->> %
                 #_(flock-near)
                 (flock vehicles)
                 (beh/move-vehicle)
                 (beh/borders width height VEHICLE-R))
           vehicles))))

(def BIN-SIZE 20)

(defn veh-loc->bin [location]
  (let [[x y] (fvec/x-y location)]
    [(int (/ x BIN-SIZE))
     (int (/ y BIN-SIZE))]))

(defn record-flock [{:keys [vehicles] :as state}]
  (let [new-locs (doall
                  (reduce (fn [memo v]
                            (let [bin (veh-loc->bin (:location v))]
                              (update-in memo [bin] #(conj (or % []) v))))
                          {} vehicles))]
    (-> state
        (assoc-in [:vehicle-locations] new-locs))))

(defn update-vehicles [width height state]
  (let [{:keys [vehicle-locations]} state]
    (-> state
        (update-in [:vehicles] (partial flock-move width height vehicle-locations))
        (record-flock))))

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
