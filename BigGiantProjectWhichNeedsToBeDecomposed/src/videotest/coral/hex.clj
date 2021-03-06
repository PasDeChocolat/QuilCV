(ns videotest.coral.hex
  (:require
   [quil.core :as q]))


(defn grid-coords [col-bins row-bins]
  (doall
   (vec
    (for [row (range 0 row-bins)
          col (range 0 col-bins)]
      [col row]))))

(defn hex-y-offset [hex-w]
  (* (Math/sin (Math/toRadians 60))
     hex-w))

(defn draw-hex-at-origin
  "Draws a hexagon, centered at x, y."
  [hex-w half-hex-w y-offset]
  (q/begin-shape)
  (q/vertex (- hex-w) 0)
  (q/vertex (- half-hex-w) (- y-offset))
  (q/vertex half-hex-w (- y-offset))
  (q/vertex hex-w 0)
  (q/vertex half-hex-w y-offset)
  (q/vertex (- half-hex-w) y-offset)
  (q/vertex (- hex-w) 0)
  (q/end-shape))

(defn draw-hex
  "Draws a hexagon, centered at x, y."
  [hex-w half-hex-w y-offset x y]
  (q/with-translation [x y]
    (draw-hex-at-origin hex-w half-hex-w y-offset)))

(defn draw-hex-cell
  ([w half-w hex-w half-hex-w y-offset col row]
     (draw-hex-cell true
                    w half-w hex-w half-hex-w y-offset col row))
  ([odd-col-lower
    w half-w hex-w half-hex-w y-offset col row]
     (let [x (+ (* col w) half-w)
           y (+ (* row w) half-w)
           y (cond
              (and odd-col-lower (odd? col))
              (+ y half-w)
              
              (and (not odd-col-lower) (even? col))
              (+ y half-w)
              
              :default y)]
       (draw-hex hex-w half-hex-w y-offset x y))))

(defn draw-hex-grid [bin-size hex-w {:keys [grid-coords]}]
  (let [half-bin-size (/ bin-size 2.0)
        half-hex-w (/ hex-w 2.0)
        y-offset (hex-y-offset hex-w)
        draw (partial draw-hex-cell
                      bin-size half-bin-size hex-w half-hex-w y-offset)]
    (dorun
     (map (fn [[col row]]
            (draw col row))
          grid-coords))))
