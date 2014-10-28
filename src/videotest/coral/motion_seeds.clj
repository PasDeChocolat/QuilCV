(ns videotest.coral.motion-seeds
  (:require
   [quil.core :as q]
   [videotest.coral.color :as color]
   [videotest.coral.hex :as hex]
   [videotest.coral.motion-trace :as mtrace]
   [videotest.coral.noise :as pnoise]))


(def SEED-W 10)
(def SEED-CREATE-ODDS 0.1)

(def SEED-X-VEL-BOUND 20.0)
(def SEED-Y-VEL-BOUND-MIN 5.0)
(def SEED-Y-VEL-BOUND-MAX 20.0)


(defn seed-created? []
  (> SEED-CREATE-ODDS (rand)))

(defn init-motion-seed [x y color]
  {:x x :y y :color color})

(defn create-motion-seeds [bin-size {:keys [motion-trace motion-seeds color-record] :as state}]
  (assoc-in state [:motion-seeds]
            (reduce (fn [memo [[col row :as coords] {:keys [life]}]]
                      (if (and (<= mtrace/MAX-TRACE-LIFE life)
                               (seed-created?))
                        (conj memo (init-motion-seed (+ (* col bin-size)
                                                        (rand bin-size))
                                                     (+ (* row bin-size)
                                                        (rand bin-size))
                                                     (color-record coords)))
                        memo))
                    motion-seeds
                    motion-trace)))

(defn move-motion-seeds [display-height {:keys [motion-seeds] :as state}]
  (assoc-in state [:motion-seeds]
            (reduce (fn [memo {:keys [x y] :as seed}]
                      (let [p-noise (pnoise/xy->perlin x y)
                            x (+ (pnoise/perlin->range p-noise (- SEED-X-VEL-BOUND) SEED-X-VEL-BOUND) x)
                            y (+ (pnoise/perlin->range p-noise SEED-Y-VEL-BOUND-MIN SEED-Y-VEL-BOUND-MAX) y)]
                        (if (< (+ display-height SEED-W) y)
                          memo
                          (conj memo (-> seed
                                         (assoc-in [:p-noise] p-noise)
                                         (assoc-in [:x] x)
                                         (assoc-in [:y] y))))))
                    []
                    motion-seeds)))

(defn update-motion-seeds [bin-size display-height state]
  (->> state
       (create-motion-seeds bin-size)
       (move-motion-seeds display-height)))

(defn draw-hex-motion-seed [hex-w half-hex-w y-offset p-noise x y color]
  (let [alpha (pnoise/perlin->range p-noise 100.0 200.0)
        c (color/color-with-alpha color alpha)]
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
       (map (fn [{:keys [p-noise x y color]}]
              (draw p-noise x y color))
            motion-seeds)))
    (q/pop-style))