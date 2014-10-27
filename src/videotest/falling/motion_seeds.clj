(ns videotest.falling.motion-seeds
  (:require
   [quil.core :as q]
   [videotest.falling.motion-trace :as mtrace]))


(def SEED-W 20)
(def SEED-CREATE-ODDS 0.1)

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
                      (let [x (+ (- (rand 20) 10) x)
                            y (+ 10 y)]
                        (if (< (+ display-height SEED-W) y)
                          memo
                          (conj memo (-> seed
                                         (assoc-in [:x] x)
                                         (assoc-in [:y] y))))))
                    []
                    motion-seeds)))

(defn update-motion-seeds [bin-size display-height state]
  (->> state
       (create-motion-seeds bin-size)
       (move-motion-seeds display-height)))

(defn draw-motion-seed [x y color]
  (apply q/fill color)
  (q/ellipse x y SEED-W SEED-W))

(defn draw-motion-seeds [{:keys [motion-seeds]}]
  (q/push-style)
  (q/no-stroke)
  (dorun
   (map (fn [{:keys [x y color]}]
          (draw-motion-seed x y color))
        motion-seeds))
  (q/pop-style))
