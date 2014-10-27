(ns videotest.falling.motion-trace
  (:require
   [quil.core :as q]
   [clojure.math.numeric-tower :as math]
   [videotest.falling.cv-draw :as cv-draw]))

(def MAX-TRACE-LIFE 60)

(defn init-trace []
  {:life MAX-TRACE-LIFE})

(defn color-with-alpha [c alpha]
  (conj (vec (take 3 c)) alpha))

(defn color-changed? [current-color previous-color]
  (if (nil? previous-color)
    false
    (let [changes (map (comp math/abs -)
                       current-color previous-color)
          channel-change-big-enough? #(< 50 %)]
      (some channel-change-big-enough? changes))))

(defn build-motion-trace
  [{:keys [motion-trace triangle-points color-record previous-color-record] :as state}]
  (let [inc-life (fn [trace]
                   (let [trace (if (empty? trace)
                                 (init-trace)
                                 trace)]
                     (update-in trace [:life] #(min MAX-TRACE-LIFE
                                                    (+ 10 %)))))]
    (assoc-in state [:motion-trace]
              (reduce (fn [memo coords]
                        (if (color-changed? (color-record coords) (previous-color-record coords))
                          (update-in memo [coords] inc-life)
                          memo))
                      motion-trace
                      (keys triangle-points)))))

(defn decay-motion-trace
  [{:keys [motion-trace triangle-points color-record previous-color-record] :as state}]
  (let [dec-life (fn [trace]
                   (update-in trace [:life] #(max 0 (- % (+ 1 (rand 10))))))]
    (assoc-in state [:motion-trace]
              (reduce (fn [memo [coords trace]]
                        (if (>= 1 (:life trace))
                          (dissoc memo coords)
                          (update-in memo [coords] dec-life)))
                      motion-trace
                      motion-trace))))

(defn update-motion-trace [state]
  (-> state
      (decay-motion-trace)
      (build-motion-trace)))

(defn overlay-single-trace
  [tri-points tri-glyphs color-record drawn-mat coords life]
  (let [[display-x display-y] (tri-points coords)
        c (color-with-alpha (color-record coords) (q/map-range life 0 MAX-TRACE-LIFE 0 255))
        pts (tri-glyphs coords)]
    (cv-draw/draw-partial-poly-outline-with-pts drawn-mat c pts)))

(defn overlay-motion-trace
  [{:keys [motion-trace triangle-points triangle-glyphs color-record drawn-mat] :as state}]
  (dorun
   (map (fn [[coords {:keys [life]}]]
          (if (< 0 life)
            (overlay-single-trace triangle-points
                                  triangle-glyphs
                                  color-record
                                  drawn-mat
                                  coords
                                  life)))
        motion-trace))
  state)
