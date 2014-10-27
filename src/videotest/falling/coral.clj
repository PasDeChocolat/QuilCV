(ns videotest.falling.coral
  (:require
   [quil.core :as q]
   [clojure.math.numeric-tower :as math]
   [videotest.falling.cv-draw :as cv-draw]))

(def MAX-CORAL-LIFE 60)

(defn init-polyp []
  {:life (inc (rand-int MAX-CORAL-LIFE))})

(defn color-with-alpha [c alpha]
  (conj (vec (take 3 c)) alpha))

(defn color-changed? [current-color previous-color]
  (if (nil? previous-color)
    false
    (let [changes (map (comp math/abs -)
                       current-color previous-color)
          channel-change-big-enough? #(< 50 %)]
      (some channel-change-big-enough? changes))))

(defn build-coral
  [{:keys [coral triangle-points color-record previous-color-record] :as state}]
  (let [inc-life (fn [polyp]
                   (let [polyp (if (empty? polyp)
                                 (init-polyp)
                                 polyp)]
                     (update-in polyp [:life] #(min MAX-CORAL-LIFE
                                                    (+ 10 %)))))]
    (assoc-in state [:coral]
              (reduce (fn [memo coords]
                        (if (color-changed? (color-record coords) (previous-color-record coords))
                          (update-in memo [coords] inc-life)
                          memo))
                      coral
                      (keys triangle-points)))))

(defn decay-coral
  [{:keys [coral triangle-points color-record previous-color-record] :as state}]
  (let [dec-life (fn [polyp]
                   (update-in polyp [:life] #(max 0 (dec %))))]
    (assoc-in state [:coral]
              (reduce (fn [memo [coords polyp]]
                        (if (>= 1 (:life polyp))
                          (dissoc memo coords)
                          (update-in memo [coords] dec-life)))
                      coral
                      coral))))

(defn update-coral [state]
  (-> state
      (build-coral)
      (decay-coral)))

(defn overlay-coral-piece
  [tri-points tri-glyphs color-record drawn-mat coords life]
  (let [[display-x display-y] (tri-points coords)
        c (color-with-alpha (color-record coords) (q/map-range life 0 MAX-CORAL-LIFE 0 255))
        pts (tri-glyphs coords)]
    (cv-draw/draw-partial-poly-outline-with-pts drawn-mat c pts)))

(defn overlay-coral
  [{:keys [coral triangle-points triangle-glyphs color-record drawn-mat] :as state}]
  (dorun
   (map (fn [[coords {:keys [life]}]]
          (if (< 0 life)
            (overlay-coral-piece triangle-points
                                 triangle-glyphs
                                 color-record
                                 drawn-mat
                                 coords
                                 life)))
        coral))
  state)
