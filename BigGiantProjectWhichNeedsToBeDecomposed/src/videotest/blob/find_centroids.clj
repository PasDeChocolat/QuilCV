(ns videotest.blob.find-centroids
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.basic-mover.cv :as cv])
  (:import
   [org.opencv.core CvType Mat]
   [blobscanner Detector]))

;; Based on Blobscanner example sketch by Antonio Molinaro:
;; https://github.com/robdanet/blobscanner/blob/master/examples/blob_centroid/findCentroids/findCentroids.pde
;;
;; 

(def CAM-SIZE (cv/camera-frame-size))
(def WIDTH  (int (:width  CAM-SIZE)))
(def HEIGHT (int (:height CAM-SIZE)))

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))

(def this (atom nil))

;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  {:b-array (byte-array PIX-CNT1)
   :i-array (int-array PIX-CNT2)
   :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC3)
   :output-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
   :camera (cv/camera 0)
   :p-image (q/create-image WIDTH HEIGHT :rgb)
   :detector (Detector. @this 255)
   })

(defn p-image->bw [p-image]
  (q/image-filter p-image :threshold)
  p-image)

(defn update-blobs [{:keys [detector p-image] :as state}]
  (.imageFindBlobs detector p-image)
  (.loadBlobsFeatures detector)
  (.findCentroids detector)
  state)

(defn update [state]
  (-> state
      (cv/update-frame)
      (cv/update-p-image)
      (update-in [:p-image] p-image->bw)
      (update-blobs)))

(defn draw-centroids [{:keys [detector]}]
  (q/push-style)
  (q/stroke-weight 5)
  (q/stroke 0 255 0)
  (if (< 0 (.getBlobsNumber detector))
   (q/point (.getCentroidX detector 0)
            (.getCentroidY detector 0)))
  (q/pop-style))

(defn draw [state]
  (let [{:keys [p-image]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate WIDTH 0)
    (q/scale -1 1)
    (q/image p-image 0 0)
    (draw-centroids state)
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

(reset! this videotest)
