(ns videotest.triangles.triangle-core
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [clj-yaml.core :as yaml]
   [videotest.triangles.cv :as cv])
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
      #_(assoc-in state [:gray-mat]
                (cv/gray-mat->otsu-gaussian-bw gray-mat bw-mat
                                               (:output-mat state)
                                               {:threshold-type Imgproc/THRESH_BINARY
                                                :blur-k-size (Size. 41 41)}))
      (assoc-in state [:gray-mat] gray-mat))
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
      #_(update-gray-p-image)
      #_(update-blobs)))

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

(def MOSAIC-BIN-SIZE 16)
(def MOSAIC-BIN-SIZE-X2 (* MOSAIC-BIN-SIZE 2.0))
(def MOSAIC-BIN-SIZE-2 (/ MOSAIC-BIN-SIZE 2.0))
(def NUM-COL-BINS (/ WIDTH  MOSAIC-BIN-SIZE))
(def NUM-ROW-BINS (/ HEIGHT MOSAIC-BIN-SIZE))

(def PI_2   (/ Math/PI 2.0))
(def PI_3_2 (* 1.5 Math/PI))
(def ORIENTATIONS [0 PI_2 Math/PI PI_3_2])

(defn draw-mosaic-glyph [gray
                         rotation
                         mat-col mat-row]
  (q/fill gray 255)
  (q/with-translation [(+ mat-col MOSAIC-BIN-SIZE)
                       (+ mat-row MOSAIC-BIN-SIZE)]
    (q/with-rotation [rotation]
      (q/with-translation [(- MOSAIC-BIN-SIZE)
                           (- MOSAIC-BIN-SIZE)]
       (q/triangle 0                  0
                   0                  MOSAIC-BIN-SIZE-X2
                   MOSAIC-BIN-SIZE-X2 MOSAIC-BIN-SIZE-X2)))))

(defn gray-threshold [gray-mat mat-col mat-row]
  (let [g (.get gray-mat mat-row mat-col)
        g (if (< 0 (count g))
            (first g)
            0)]
    (if (< 126 g)
      g
      0)))

(defn draw-mosaic-pair [gray-mat [pt1 pt2]]
  (let [gray-fn (fn [mat-col mat-row]
                  (let [g (.get gray-mat mat-row mat-col)]
                    (if (< 0 (count g))
                      (first g)
                      0)))
        ;gray-fn (partial gray-threshold gray-mat)
        [mat-col1 mat-row1] pt1
        [mat-col2 mat-row2] pt2
        g1 (gray-fn mat-col1 mat-row1)
        g2 (gray-fn mat-col2 mat-row2)
        rotation1 (nth ORIENTATIONS (rand-int 3))
        rotation2 (+ rotation1 Math/PI)]
    (draw-mosaic-glyph g1 rotation1 mat-col1 mat-row1)
    (draw-mosaic-glyph g2 rotation2 mat-col1 mat-row1)))

(defn draw-mosaic [gray-mat]
  (q/push-matrix)
  (q/push-style)
  (q/no-stroke)
  (dorun
   (map (partial draw-mosaic-pair gray-mat)
        (partition 2
                   (for [col-bin (range 0 NUM-COL-BINS)
                         row-bin (range 0 NUM-ROW-BINS)
                         :let [mat-col (+ (* col-bin MOSAIC-BIN-SIZE)
                                          MOSAIC-BIN-SIZE-2)
                               mat-row (+ (* row-bin MOSAIC-BIN-SIZE)
                                          MOSAIC-BIN-SIZE-2)]
                         :when (= 0 (mod col-bin 2))]
                     [mat-col mat-row]))))
  (q/pop-style)
  (q/pop-matrix))

(defn draw [state]
  (let [{:keys [p-image gray-mat]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate WIDTH 0)
    (q/scale -1 1)
    (when p-image
      #_(q/image p-image 0 0)
      (when-not (cv/mat-empty? gray-mat)
        (draw-mosaic gray-mat))
      #_(draw-centroids state))
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
