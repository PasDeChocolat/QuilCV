(ns videotest.basic-loop
  (:require
   [quil.core :as q]
   [quil.middleware :as m])
  (:import
   [org.opencv.highgui Highgui VideoCapture]
   [org.opencv.core Mat]))

(def WIDTH 640)
(def HEIGHT 480)

;; OpenCV Implementation Wrapper - Will move to separate NS.
(defn camera [dev]
  (VideoCapture. dev))

(defn is-opened? [cam]
  (.isOpened cam))

(defn frame-mat []
  (Mat.))

(defn grab-frame! [cam frame]
  (.read cam frame)
  frame)

(defn get-mat-pix [mat-pix i]
  (bit-and (nth mat-pix i)
           0xFF))

(defn mat->p-img
  ([mat]
     (let [w (.width mat)
           h (.height mat)
           p-img (q/create-image w h :argb)]
       (mat->p-img mat p-img)))
  ([mat p-img]
     (.loadPixels p-img)
     (condp = (.channels mat)
       3 (let [channels 3
               w (.width mat)
               h (.height mat)
               n (* w h channels)
               mat-pixels (byte-array n)]
           (.get mat 0 0 mat-pixels)
           (dorun
            (for [i (range 0 n channels)
                  :let [c (q/color
                           (get-mat-pix mat-pixels (+ i 2))
                           (get-mat-pix mat-pixels (+ i 1))
                           (get-mat-pix mat-pixels i))]]
              (aset (.pixels p-img) (int (/ i channels)) c))))
       nil)
     (.updatePixels p-img)
     p-img))

;; <--End OpenCV Implementation Wrapper

(defn update-p-image [state]
  (let [{:keys [frame-mat]} state]
    (update-in state [:p-image] #(mat->p-img frame-mat %))))

(defn update-frame [state]
  (let [{:keys [camera]} state]
    (cond-> state
            (is-opened? camera)
            (update-in [:frame-mat] (partial grab-frame! camera) ))))

(defn setup []
  (q/frame-rate 2)
  ;(q/color-mode :hsb)
  {:frame-mat (frame-mat)
   :camera (camera 0)
   :p-image (q/create-image WIDTH HEIGHT :argb)})

(defn update
  "Update sketch state by changing circle color and position."
  [{:keys [camera] :as state}]
  (-> state
      (update-frame)
      (update-p-image)))

(defn draw [state]
  (let [{:keys [p-image]} state]
    (q/image p-image 0 0)))

(defn on-close
  ([{:keys [camera]}]
     (println "closing sketch and releasing camera...")
     (when-not (nil? camera)
       (.release camera))))

(q/defsketch videotest
  :title "Video Test"
  :size [WIDTH HEIGHT]
  :setup setup
  ;; something for on-close??? (.release camera)
  :update update
  :draw draw
  :on-close on-close
  :middleware [m/fun-mode])
