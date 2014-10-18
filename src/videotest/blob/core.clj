(ns videotest.blob.core
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.basic-mover.cv :as cv])
  (:import
   [org.opencv.core CvType Mat MatOfByte MatOfFloat MatOfPoint MatOfPoint2f]))


(def CAM-SIZE (cv/camera-frame-size))
(def WIDTH  (int (:width  CAM-SIZE)))
(def HEIGHT (int (:height CAM-SIZE)))

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))


;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  {:b-array (byte-array PIX-CNT1)
   :i-array (int-array PIX-CNT2)
   :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC3)
   :output-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
   :camera (cv/camera 0)
   :p-image (q/create-image WIDTH HEIGHT :rgb)})

(defn update [state]
  (-> state
      (cv/update-frame)
      (cv/update-p-image)
      #_(cv/init-gray-mat)
      #_(cv/update-optical-flow)))

(defn draw [state]
  (let [{:keys [p-image]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate WIDTH 0)
    (q/scale -1 1)
    (q/image p-image 0 0)
    #_(draw-flow state)
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
