(ns videotest.hex.core
  (:require
   [quil.applet :as qa :refer [applet-close]]
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.hex.hex :as hex]))

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

(defn setup []
  {:grid-coords (grid-coords NUM-COL-BINS NUM-ROW-BINS)})

(defn update [state]
  state)

(defn draw [state]
  (q/background 0)
  (q/push-matrix)
  (q/translate DISPLAY-WIDTH 0)
  (q/scale -1 1)
  (q/no-fill)
  (q/stroke-weight 1)
  (q/stroke 0 255 255 255)
  (hex/draw-hex-grid DISPLAY-BIN-SIZE HEX-W state)
  (q/pop-matrix))

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
