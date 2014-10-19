(ns videotest.blob.find-centroids-cvgray
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.blob.cv :as cv])
  (:import
   [org.opencv.core CvType Mat Size]
   [org.opencv.imgproc Imgproc]
   [java.nio ByteBuffer ByteOrder]
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

(def BW-THRESH 50) ; between 0-255 (gray -> BW cut-off)
(def this (atom nil))

;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  {:b-array (byte-array PIX-CNT1)
   :i-array (int-array PIX-CNT2)
   :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC3)
   :output-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
   :gray-mat (Mat.)
   :bw-mat (Mat.)
   :camera (cv/camera 0)
   :p-image (q/create-image WIDTH HEIGHT :rgb)
   :detector (do
               ; Applet must exist before Detector can be created
               (Thread/sleep 1000)
               (Detector. @this 255))})

(defn update-gray-mat
  [{:keys [frame-mat gray-mat bw-mat] :as state}]
  (if frame-mat
    (let [gray-mat (cv/BGR->GrayMat! frame-mat gray-mat)]
      #_(assoc-in state [:gray-mat] (cv/gray-mat->bw-mat gray-mat bw-mat
                                                         {:bw-threshold BW-THRESH}))
      #_(assoc-in state [:gray-mat] (cv/gray-mat->inv-bw-mat gray-mat bw-mat
                                                             {:bw-threshold BW-THRESH}))
      #_(assoc-in state [:gray-mat]
                (cv/gray-mat->adaptive-gaussian-bw gray-mat bw-mat
                                               {:threshold-type Imgproc/THRESH_BINARY_INV
                                                :block-size 5
                                                :c 2}))
      (assoc-in state [:gray-mat]
                (cv/gray-mat->otsu-gaussian-bw gray-mat bw-mat
                                               (:output-mat state)
                                               {:threshold-type Imgproc/THRESH_BINARY_INV
                                                :blur-k-size (Size. 61 61)})))
    state))

(defn update-gray-p-image
  [{:keys [gray-mat output-mat b-array i-array] :as state}]
  (if gray-mat
    (update-in state [:p-image] #(cv/gray-mat->p-img gray-mat output-mat b-array i-array %))
    state))

(defn update-blobs [{:keys [detector p-image] :as state}]
  (when p-image
    (.imageFindBlobs detector p-image)
    (.loadBlobsFeatures detector)
    (.findCentroids detector))
  state)

(defn update [state]
  (-> state
      (cv/update-frame)
      (update-gray-mat)
      (update-gray-p-image)
      (update-blobs)))

(defn draw-centroid [detector n]
  (q/ellipse (.getCentroidX detector n)
             (.getCentroidY detector n)
             40 40))

(defn draw-centroids [{:keys [detector]}]
  (when (and detector
             (< 0 (.getBlobsNumber detector)))
    (q/push-style)
    (q/no-stroke)
    (q/fill 0 255 0 80)
    (dorun
     (map #(draw-centroid detector %)
          (range 0 (.getBlobsNumber detector))))
    (q/pop-style)))

(defn draw [state]
  (let [{:keys [p-image]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate WIDTH 0)
    (q/scale -1 1)
    (when p-image
      (q/image p-image 0 0)
      (draw-centroids state))
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
