(ns videotest.coral.motion-seeds
  (:require
   [quil.core :as q]
   [videotest.coral.color :as color]
   [videotest.coral.flocking-seeds :as fseed]
   [videotest.coral.hex :as hex]
   [videotest.coral.motion-trace :as mtrace]
   [videotest.coral.noise :as pnoise]))


;; (def SEED-W 10)
;; (def SEED-CREATE-ODDS 0.1)
(def SEED-W 6)
;; (def SEED-CREATE-ODDS 0.4)
(def SEED-CREATE-ODDS 0.1) ;; slow flocking

(def SEED-X-VEL-BOUND 20.0)
;; (def SEED-Y-VEL-BOUND-MIN 5.0)
;; (def SEED-Y-VEL-BOUND-MAX 20.0)
(def SEED-Y-VEL-BOUND-MIN 2.0)
(def SEED-Y-VEL-BOUND-MAX 12.0)


(defn seed-created? []
  (> SEED-CREATE-ODDS (rand)))

(defn rand-target-velocity []
  [(q/map-range (rand)
                0.0 1.0
                (- SEED-X-VEL-BOUND)
                SEED-X-VEL-BOUND)
   (q/map-range (rand)
                0.0 1.0
                SEED-Y-VEL-BOUND-MIN
                SEED-Y-VEL-BOUND-MAX)])

(defn init-motion-seed [rgb-in-mat hsv-out-mat x y rgba]
  (let [hsva (color/rgba->hsva rgb-in-mat hsv-out-mat rgba)
        location [x y]
        velocity (rand-target-velocity)]
    (-> (fseed/init-flocking-seed location velocity rgba hsva)
        (assoc-in [:target-velocity] velocity))))

(defn create-motion-seeds [bin-size {:keys [motion-trace motion-seeds color-record] :as state}]
  (let [rgb-in-mat (color/single-three-dim-color-mat)
        hsv-out-mat (color/single-three-dim-color-mat)]
    (assoc-in state [:motion-seeds]
           (reduce (fn [memo [[col row :as coords] {:keys [life]}]]
                     (if (and (<= mtrace/MAX-TRACE-LIFE life)
                              (seed-created?))
                       (conj memo (init-motion-seed rgb-in-mat hsv-out-mat
                                                    (+ (* col bin-size)
                                                       (rand bin-size))
                                                    (+ (* row bin-size)
                                                       (rand bin-size))
                                                    (color-record coords)))
                       memo))
                   motion-seeds
                   motion-trace))))

(defn out-of-bounds? [seed-w display-w display-h x y]
  (or (< (+ display-h seed-w) y)
      (> 0 (+ seed-w) x)
      (< display-w (- x seed-w))))

(def SEED-VELOCITY-LERP 0.7)
(def SEED-VELOCITY-CHANGE-CYCLE 5)

(defn move-motion-seeds [display-w display-h {:keys [motion-seeds] :as state}]
  (let [reduce-seed
        (fn [memo seed]
          (let [[x y] (fseed/loc seed)
                p-noise (pnoise/xy->perlin x y)]
            (if (out-of-bounds? SEED-W display-w display-h x y)
              memo
              (conj memo (-> seed
                             (assoc-in [:p-noise] p-noise))))))]
    (-> state
        (fseed/update-vehicles display-w display-h)
        (update-in [:motion-seeds] #(reduce reduce-seed [] %)))))

(defn update-motion-seeds [bin-size display-w display-h state]
  (->> state
       (create-motion-seeds bin-size)
       (move-motion-seeds display-w display-h)))

(defn draw-hex-motion-seed [hex-w half-hex-w y-offset p-noise location color-rgba]
  (let [alpha (pnoise/perlin->range p-noise 100.0 200.0)
        [x y] location
        c (color/color-with-alpha color-rgba alpha)]
    (apply q/fill c)
    (q/with-translation [x y]
      (q/with-rotation [(* p-noise Math/PI)]
        (hex/draw-hex-at-origin hex-w half-hex-w y-offset)))))

(defn draw-motion-seeds [{:keys [motion-seeds]}]
    (q/push-style)
    (q/no-stroke)
    (let [hex-w SEED-W
          half-hex-w (/ hex-w 2.0)
          y-offset (hex/hex-y-offset hex-w)
          draw (partial draw-hex-motion-seed
                        hex-w half-hex-w y-offset)]
      (dorun
       (map (fn [{:keys [p-noise color-rgba] :as seed}]
              (let [location (fseed/loc seed)]
                (draw p-noise location color-rgba)))
            motion-seeds)))
    (q/pop-style))
