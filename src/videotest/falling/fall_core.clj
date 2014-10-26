(ns videotest.falling.fall-core
  (:require
   [quil.applet :as qa :refer [applet-close]]
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.falling.cv :as cv]
   [videotest.falling.cv-draw :as cv-draw]
   [videotest.falling.triangles :as tri])
  (:import
   [org.opencv.core Core CvType Mat MatOfKeyPoint Point Size]
   [org.opencv.features2d FeatureDetector KeyPoint]
   [org.opencv.imgproc Imgproc]
   [java.nio ByteBuffer ByteOrder]))

(def CAM-DEV-NUM 0)
(def CAM-SIZE (cv/camera-frame-size CAM-DEV-NUM))
(def CAM-WIDTH  (int (:width  CAM-SIZE)))
(def CAM-HEIGHT (int (:height CAM-SIZE)))

;; Optoma Projector
;; (def DISPLAY-WIDTH 1280.0)
;; (def DISPLAY-HEIGHT 800.0)

;; Lenovo
(def DISPLAY-WIDTH 1366.0)
(def DISPLAY-HEIGHT 768.0)

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (int (* DISPLAY-WIDTH DISPLAY-HEIGHT 4)))
(def PIX-CNT2 (int (* DISPLAY-WIDTH DISPLAY-HEIGHT)))

(def ALPHA-STILL 255.0)

;; 32 works
(def NUM-COL-BINS 64.0)
(def DISPLAY-BIN-SIZE (/ DISPLAY-WIDTH NUM-COL-BINS))
(def NUM-ROW-BINS (/ DISPLAY-HEIGHT DISPLAY-BIN-SIZE))

(def DISPLAY-BIN-SIZE-X2  (* DISPLAY-BIN-SIZE 2.0))
(def DISPLAY-BIN-SIZE-2   (/ DISPLAY-BIN-SIZE 2.0))

(def CAM-BIN-SIZE   (/ CAM-WIDTH NUM-COL-BINS))
(def CAM-BIN-SIZE-2 (/ CAM-BIN-SIZE 2.0))
(defn display->cam [display-x-or-y]
  (* display-x-or-y (/ CAM-WIDTH DISPLAY-WIDTH)))


(defn mat
  ([]
     (Mat.))
  ([h w t]
     (Mat. (int h) (int w) t)))

;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  (let [tri-pts     (tri/triangle-points NUM-COL-BINS NUM-ROW-BINS
                                         DISPLAY-BIN-SIZE)
        tri-orients (tri/triangle-orientations tri-pts)
        tri-glyphs  (tri/triangle-glyphs DISPLAY-BIN-SIZE-X2
                                         tri-pts tri-orients)]
   {:b-array (byte-array PIX-CNT1)
    :i-array (int-array PIX-CNT2)
    :frame-mat  (mat CAM-HEIGHT CAM-WIDTH CvType/CV_8UC3)
    :output-mat (mat DISPLAY-HEIGHT DISPLAY-WIDTH CvType/CV_8UC4)
    :gray-mat   (mat)
    :rgba-mat   (mat CAM-HEIGHT CAM-WIDTH CvType/CV_8UC4)
    :drawn-mat  (mat DISPLAY-HEIGHT DISPLAY-WIDTH CvType/CV_8UC4)
    :camera  (cv/camera CAM-DEV-NUM)
    :p-image (q/create-image DISPLAY-WIDTH DISPLAY-HEIGHT :rgb)
    :triangle-points       tri-pts
    :triangle-orientations tri-orients
    :triangle-glyphs       tri-glyphs
    :color-record {}}))

(defn update-rgba [{:keys [rgba-mat frame-mat] :as state}]
  (assoc-in state [:rgba-mat] (cv/BGR->RGBA! frame-mat rgba-mat)))

(defn update-color-record [{:keys [rgba-mat color-record] :as state}]
  #_(dorun
     (map ))
  state)

(defn draw-mosaic
  [tri-points tri-glyphs drawn-mat rgba-mat [col row :as coords]]
  (let [color-fn (fn [display-x display-y]
                   (let [cam-x (display->cam display-x)
                         cam-y (display->cam display-y)
                         cam-x (+ cam-x CAM-BIN-SIZE-2)
                         cam-y (+ cam-y CAM-BIN-SIZE)
                         c (.get rgba-mat
                                 cam-y
                                 cam-x)]
                     (if (< 0 (count c))
                       (vec c)
                       [0 0 0 255])))
        [display-x display-y] (tri-points coords)
        c (color-fn display-x display-y)]
    (cv-draw/draw-poly-with-pts drawn-mat c (tri-glyphs coords))))

(defn overlay-triangles
  [{:keys [drawn-mat rgba-mat triangle-points triangle-glyphs] :as state}]
  (dorun
   (map (partial draw-mosaic
                 triangle-points
                 triangle-glyphs
                 drawn-mat
                 rgba-mat)
        (keys triangle-points)))
  state)

(defn update-drawn-p-image [state]
  (let [{:keys [drawn-mat output-mat b-array i-array]} state]
    (update-in state [:p-image] #(cv/mat->p-img drawn-mat output-mat b-array i-array %))))

(defn update [state]
  (-> state
      (cv/update-frame)
      (update-rgba)
      (overlay-triangles)
      (update-drawn-p-image)
      (update-color-record)))

(defn draw [state]
  (let [{:keys [p-image frame-mat]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate DISPLAY-WIDTH 0)
    (q/scale -1 1)
    (when p-image
      (q/image p-image 0 0))
    (q/pop-matrix)))

(defn on-close
  ([{:keys [camera]}]
     (println "closing sketch and releasing camera...")
     (when-not (nil? camera)
       (.release camera))))

(q/defsketch videotest
  :title "Video Test"
  :size [DISPLAY-WIDTH DISPLAY-HEIGHT]
  :setup setup
  :update update
  :draw draw
  :on-close on-close
  :middleware [m/fun-mode])

  (.setResizable (.frame videotest) true)
