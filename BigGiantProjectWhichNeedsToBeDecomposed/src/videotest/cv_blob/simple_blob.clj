(ns videotest.cv-blob.simple-blob
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [clj-yaml.core :as yaml]
   [videotest.cv-blob.cv :as cv])
  (:import
   [org.opencv.core CvType Mat MatOfKeyPoint Point Size]
   [org.opencv.features2d FeatureDetector KeyPoint]
   [org.opencv.imgproc Imgproc]
   [java.nio ByteBuffer ByteOrder]))


(def CAM-SIZE (cv/camera-frame-size))
(def WIDTH  (int (:width  CAM-SIZE)))
(def HEIGHT (int (:height CAM-SIZE)))

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))

(def BW-THRESH 50) ; between 0-255 (gray -> BW cut-off)


;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  (let [detector (FeatureDetector/create FeatureDetector/SIMPLEBLOB)]
    (.read detector "detector.txt")
   {:b-array (byte-array PIX-CNT1)
    :i-array (int-array PIX-CNT2)
    :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC3)
    :output-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
    :gray-mat (Mat.)
    :bw-mat (Mat.)
    :camera (cv/camera 0)
    :p-image (q/create-image WIDTH HEIGHT :rgb)
    :detector detector
    :blob-points (MatOfKeyPoint.)}))

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
                                               {:threshold-type Imgproc/THRESH_BINARY
                                                :blur-k-size (Size. 41 41)})))
    state))

(defn update-gray-p-image
  [{:keys [gray-mat output-mat b-array i-array] :as state}]
  (if gray-mat
    (update-in state [:p-image] #(cv/gray-mat->p-img gray-mat output-mat b-array i-array %))
    state))

(defn update-blobs [{:keys [detector gray-mat blob-points] :as state}]
  (when gray-mat
    (.detect detector gray-mat blob-points))
  state)

(defn update [state]
  (-> state
      (cv/update-frame)
      (update-gray-mat)
      (update-gray-p-image)
      (update-blobs)))

(defn draw-centroid [pt]
  (q/ellipse (.x pt)
             (.y pt)
             40 40))

(defn draw-centroids [{:keys [blob-points]}]
  (let [blob-list (.toList blob-points)]
    (when (< 0 (count blob-list))
     (q/push-style)
     (q/no-stroke)
     (q/fill 0 255 0 80)
     (dorun
      (map #(draw-centroid (.pt %))
           blob-list))
     (q/pop-style))))

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
