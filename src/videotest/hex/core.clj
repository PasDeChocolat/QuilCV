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


(defn grid-coords [col-bins row-bins]
  (doall
   (vec
    (for [row (range 0 row-bins)
          col (range 0 col-bins)]
      [col row]))))

(defn draw-cell [col row w half-w]
  (q/push-matrix)
  (q/rect (+ (* col w) half-w)
          (+ (* row w) half-w)
          w w)
  (q/pop-matrix))

(defn draw-hex-grid [{:keys [grid-coords]}]
  (q/rect-mode :center)
  (q/no-fill)
  (q/stroke-weight 1)
  (q/stroke 0 255 255 255)
  (dorun
   (map (fn [[col row]]
          (draw-cell col row DISPLAY-BIN-SIZE DISPLAY-BIN-SIZE-2))
        grid-coords)))

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
