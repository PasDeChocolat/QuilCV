(ns videotest.flock-tune.flock-core
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.flock-tune.cv :as cv]
   [videotest.flock-tune.flock :as flock]
   [videotest.flock-tune.gesture :as gesture])
  (:import
   [org.opencv.core CvType Mat MatOfByte MatOfFloat MatOfPoint MatOfPoint2f]))


(def CAM-SIZE (cv/camera-frame-size))
(def WIDTH  (int (:width  CAM-SIZE)))
(def HEIGHT (int (:height CAM-SIZE)))

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))

(def NUM-VEHICLES 500)


;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  {:vehicles (flock/init-vehicles WIDTH HEIGHT NUM-VEHICLES)
   :vehicle-locations {}
   :gestures {}
   :b-array (byte-array PIX-CNT1)
   :i-array (int-array PIX-CNT2)
   :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC3)
   :output-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
   :old-gray-mat nil
   :old-corners nil
   :new-gray-mat (Mat.)
   :new-corners (MatOfPoint2f.)
   :lk-status (MatOfByte.)
   :lk-err (MatOfFloat.)
   :flow-pts []
   :camera (cv/camera 0)
   :p-image (q/create-image WIDTH HEIGHT :rgb)})

(defn update [state]
  (let [upd-vehicles (partial flock/update-vehicles WIDTH HEIGHT)]
    (-> state
        #_(cv/update-image-detection)
        #_(gesture/record-gestures)
        (upd-vehicles))))

(defn draw-line [old-pt new-pt]
  (q/line (.x old-pt) (.y old-pt)
          (.x new-pt) (.y new-pt)))

(defn draw-flow
  "Draw optical flow as lines."
  [{:keys [flow-pts]}]
  (when (seq flow-pts)
    (q/push-style)
    (q/stroke 255)
    (q/stroke-weight 0.5)
    (dorun
     (map (fn [[o n]]
            (draw-line o n))
          flow-pts))
    (q/pop-style)))

(defn draw-vehicles
  [vehicles]
  (q/push-style)
  (q/fill 0 255 0)
  (q/no-stroke)
  (doall (map flock/draw-vehicle vehicles))
  (q/pop-style))

(defn draw [state]
  (let [{:keys [p-image vehicles]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate WIDTH 0)
    (q/scale -1 1)
    #_(q/image p-image 0 0)
    #_(draw-flow state)
    (draw-vehicles vehicles)
    (q/pop-matrix)))

(defn on-close
  ([{:keys [camera]}]
     (println "closing sketch and releasing camera...")
     (when-not (nil? camera)
       (.release camera))))

(q/defsketch videotest
  :title "Video Test"
  :size [WIDTH HEIGHT]
  :setup setup
  :update update
  :draw draw
  :on-close on-close
  :middleware [m/fun-mode])
