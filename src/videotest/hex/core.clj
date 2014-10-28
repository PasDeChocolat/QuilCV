(ns videotest.hex.core
  (:require
   [quil.applet :as qa :refer [applet-close]]
   [quil.core :as q]
   [quil.middleware :as m]))

;; Optoma Projector
;; (def DISPLAY-WIDTH 1280.0)
;; (def DISPLAY-HEIGHT 800.0)

;; Lenovo
(def DISPLAY-WIDTH 1366.0)
(def DISPLAY-HEIGHT 768.0)

;; 32 works
(def NUM-COL-BINS 64.0)
(def DISPLAY-BIN-SIZE (/ DISPLAY-WIDTH NUM-COL-BINS))
(def NUM-ROW-BINS (/ DISPLAY-HEIGHT DISPLAY-BIN-SIZE))

(def DISPLAY-BIN-SIZE-X2  (* DISPLAY-BIN-SIZE 2.0))
(def DISPLAY-BIN-SIZE-2   (/ DISPLAY-BIN-SIZE 2.0))

(def HEX-W 10.0)
(def HEX-W-2 (/ HEX-W 2.0))

(defn grid-coords [col-bins row-bins]
  (doall
   (vec
    (for [row (range 0 row-bins)
          col (range 0 col-bins)]
      [col row]))))

(defn draw-cell [col row w half-w]
  (q/push-style)
  (when (= 5 col row)
    (q/fill 255 0 0 100)
    (q/stroke 255 0 0))
  (let [x (+ (* col w) half-w)
        y (+ (* row w) half-w)
        y (if (odd? col)
            (+ y half-w)
            y)]
    (q/rect x y w w))
  (q/pop-style))

(defn draw-hex-cell-dot [col row w half-w]
  (q/push-matrix)
  (let [x (+ (* col w) half-w)
        y (+ (* row w) half-w)
        y (if (odd? col)
            (+ y half-w)
            y)]
    (q/with-translation [x y]
      (q/ellipse 0 0 half-w half-w)))
  (q/pop-matrix))

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

(defn draw-hex-grid [{:keys [grid-coords]}]
  (q/push-matrix)
  (q/push-style)
  (q/rect-mode :center)
  (q/ellipse-mode :center)
  (q/no-fill)
  (q/stroke-weight 1)
  (let [y-offset (hex-y-offset HEX-W)]
   (dorun
    (map (fn [[col row]]
           (do
             (q/stroke 0 255 255 100)
             #_(draw-cell         col row DISPLAY-BIN-SIZE DISPLAY-BIN-SIZE-2)
             #_(draw-hex-cell-dot col row DISPLAY-BIN-SIZE DISPLAY-BIN-SIZE-2)
             (q/stroke 0 255 0 255)
             (draw-hex-cell col row DISPLAY-BIN-SIZE DISPLAY-BIN-SIZE-2 HEX-W HEX-W-2 y-offset)))
         grid-coords)))
  (q/pop-style)
  (q/pop-matrix))

(defn setup []
  {:grid-coords (grid-coords NUM-COL-BINS NUM-ROW-BINS)
   :hex-cells {}})

(defn update [state]
  state)

(defn draw [state]
  (let [x 1]
    (q/background 255)
    (q/push-matrix)
    (q/translate DISPLAY-WIDTH 0)
    (q/scale -1 1)
    (draw-hex-grid state)
    (q/pop-matrix)))

(defn on-close [state]
  )

(q/defsketch videotest
  :title "hex"
  :size [DISPLAY-WIDTH DISPLAY-HEIGHT]
  :setup setup
  :update update
  :draw draw
  :on-close on-close
  :middleware [m/fun-mode])

(.setResizable (.frame videotest) true)
