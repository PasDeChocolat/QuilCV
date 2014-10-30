(ns videotest.coral.motion-seeds
  (:require
   [quil.core :as q]
   [videotest.coral.color :as color]
   [videotest.coral.hex :as hex]
   [videotest.coral.motion-trace :as mtrace]
   [videotest.coral.noise :as pnoise]))


(def SEED-W 10)
;; (def SEED-CREATE-ODDS 0.1)
(def SEED-CREATE-ODDS 0.2)

(def SEED-X-VEL-BOUND 20.0)
(def SEED-Y-VEL-BOUND-MIN 5.0)
(def SEED-Y-VEL-BOUND-MAX 20.0)


(defn seed-created? []
  (> SEED-CREATE-ODDS (rand)))

(defn init-motion-seed [rgb-in-mat hsv-out-mat x y rgba]
  (let [hsva (color/rgba->hsva rgb-in-mat hsv-out-mat rgba)]
    {:x x :y y :color-rgba rgba :color-hsva hsva}))

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

(defn move-motion-seeds [display-w display-h {:keys [motion-seeds] :as state}]
  (let [reduce-seed
        (fn [memo {:keys [x y] :as seed}]
          (let [p-noise (pnoise/xy->perlin x y)
                x (+ x
                     (pnoise/perlin->range p-noise
                                           (- SEED-X-VEL-BOUND)
                                           SEED-X-VEL-BOUND))
                y (+ y
                     (pnoise/perlin->range p-noise
                                           SEED-Y-VEL-BOUND-MIN
                                           SEED-Y-VEL-BOUND-MAX))]
            (if (out-of-bounds? SEED-W display-w display-h x y)
              memo
              (conj memo (-> seed
                             (assoc-in [:p-noise] p-noise)
                             (assoc-in [:x] x)
                             (assoc-in [:y] y))))))]
    (assoc-in state [:motion-seeds]
              (reduce reduce-seed
                      []
                      motion-seeds))))

(defn update-motion-seeds [bin-size display-w display-h state]
  (->> state
       (create-motion-seeds bin-size)
       (move-motion-seeds display-w display-h)))

(defn draw-hex-motion-seed [hex-w half-hex-w y-offset p-noise x y color-rgba]
  (let [alpha (pnoise/perlin->range p-noise 50.0 150.0)
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
       (map (fn [{:keys [p-noise x y color-rgba]}]
              (draw p-noise x y color-rgba))
            motion-seeds)))
    (q/pop-style))
