(ns videotest.falling.fall-core
  (:require
   [clojure.math.numeric-tower :as math]
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

(def ALPHA-STILL 100.0)

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
    :color-record {}
    :previous-color-record {}}))

(defn update-rgba [{:keys [rgba-mat frame-mat] :as state}]
  (assoc-in state [:rgba-mat] (cv/BGR->RGBA! frame-mat rgba-mat)))

(defn update-color-record
  [{:keys [triangle-points rgba-mat color-record] :as state}]
  (let [cam->color (fn [col row]
                     (let [cam-x (+ (* col CAM-BIN-SIZE)
                                    CAM-BIN-SIZE-2)
                           cam-y (+ (* row CAM-BIN-SIZE)
                                    CAM-BIN-SIZE)]
                       (vec (.get rgba-mat cam-y cam-x))))
        colors (reduce (fn [memo [col row :as coords]]
                         (assoc-in memo [coords] (cam->color col row)))
                       {}
                       (keys triangle-points))]
    (assoc-in state [:color-record] colors)))

(defn update-previous-color-record
  [{:keys [color-record] :as state}]
  (assoc-in state [:previous-color-record] color-record))

(defn color-changed? [current-color previous-color]
  (if (nil? previous-color)
    false
    (let [changes (map (comp math/abs -)
                       current-color previous-color)
          channel-change-big-enough? #(< 50 %)]
      (some channel-change-big-enough? changes))))

(defn color-with-alpha [c alpha]
  (conj (vec (take 3 c)) alpha))

(defn mosaic-color [current-color previous-color]
  (if (color-changed? current-color previous-color)
    current-color
    (color-with-alpha current-color ALPHA-STILL)))

(defn draw-mosaic
  [tri-points tri-glyphs drawn-mat
   color-record previous-color-record
   [col row :as coords]]
  (let [[display-x display-y] (tri-points coords)
        c (mosaic-color (color-record coords) (previous-color-record coords))]
    (cv-draw/draw-poly-with-pts drawn-mat c (tri-glyphs coords))))

(defn overlay-triangles
  [{:keys [drawn-mat rgba-mat triangle-points triangle-glyphs
           color-record previous-color-record] :as state}]
  (dorun
   (map (partial draw-mosaic
                 triangle-points
                 triangle-glyphs
                 drawn-mat
                 color-record
                 previous-color-record)
        (keys triangle-points)))
  state)

(defn update-drawn-p-image [state]
  (let [{:keys [drawn-mat output-mat b-array i-array]} state]
    (update-in state [:p-image] #(cv/mat->p-img drawn-mat output-mat b-array i-array %))))

(defn update [state]
  (-> state
      (cv/update-frame)
      (update-rgba)
      (update-color-record)
      (overlay-triangles)
      (update-drawn-p-image)
      (update-previous-color-record)))

(defn draw [state]
  (let [{:keys [p-image frame-mat]} state]
    (q/background 255)
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
