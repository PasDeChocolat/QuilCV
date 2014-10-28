(ns videotest.hex.hex
  (:require
   [quil.applet :as qa :refer [applet-close]]
   [quil.core :as q]
   [quil.middleware :as m]))


(defn grid-coords [col-bins row-bins]
  (doall
   (vec
    (for [row (range 0 row-bins)
          col (range 0 col-bins)]
      [col row]))))

(defn hex-y-offset [hex-w]
  (* (Math/sin (Math/toRadians 60))
     hex-w))

(defn draw-hex-cell [col row w half-w hex-w half-hex-w y-offset]
  (q/push-matrix)
  (let [x (+ (* col w) half-w)
        y (+ (* row w) half-w)
        y (if (odd? col)
            (+ y half-w)
            y)]
    (q/with-translation [x y]
      (q/begin-shape)
      (q/vertex (- hex-w) 0)
      (q/vertex (- half-hex-w) (- y-offset))
      (q/vertex half-hex-w (- y-offset))
      (q/vertex hex-w 0)
      (q/vertex half-hex-w y-offset)
      (q/vertex (- half-hex-w) y-offset)
      (q/vertex (- hex-w) 0)
      (q/end-shape)))
  (q/pop-matrix))

(defn draw-hex-grid [bin-size hex-w {:keys [grid-coords]}]
  (q/push-matrix)
  (q/push-style)
  (q/rect-mode :center)
  (q/ellipse-mode :center)
  (q/no-fill)
  (q/stroke-weight 1)
  (let [half-bin-size (/ bin-size 2.0)
        half-hex-w (/ hex-w 2.0)
        y-offset (hex-y-offset hex-w)]
    (dorun
     (map (fn [[col row]]
            (do
              (q/stroke 0 255 255 100)
              (q/stroke 0 255 0 255)
              (draw-hex-cell col row bin-size half-bin-size hex-w half-hex-w y-offset)))
          grid-coords)))
  (q/pop-style)
  (q/pop-matrix))
