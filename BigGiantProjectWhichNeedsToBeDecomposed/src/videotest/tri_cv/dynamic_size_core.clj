(ns videotest.tri-cv.dynamic-size-core
  (:require
   [quil.applet :as qa :only [applet-close]]
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.tri-cv.cv :as cv])
  (:import
   [org.opencv.core Core CvType Mat MatOfKeyPoint MatOfPoint Point Scalar Size]
   [org.opencv.features2d FeatureDetector KeyPoint]
   [org.opencv.imgproc Imgproc]
   [java.nio ByteBuffer ByteOrder]
   [java.util ArrayList]))

(def CAM-DEV-NUM 1)
(def CAM-SIZE (cv/camera-frame-size CAM-DEV-NUM))
(def CAM-WIDTH  (int (:width  CAM-SIZE)))
(def CAM-HEIGHT (int (:height CAM-SIZE)))

(def DISPLAY-WIDTH 1280.0)
(def DISPLAY-HEIGHT 800.0)

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (int (* DISPLAY-WIDTH DISPLAY-HEIGHT 4)))
(def PIX-CNT2 (int (* DISPLAY-WIDTH DISPLAY-HEIGHT)))

(def ALPHA-STILL 255.0)

;; 32 works
(def NUM-COL-BINS 32.0)
(def DISPLAY-BIN-SIZE (/ DISPLAY-WIDTH NUM-COL-BINS))
(def NUM-ROW-BINS (/ DISPLAY-HEIGHT DISPLAY-BIN-SIZE))

(def DISPLAY-BIN-SIZE-X2  (* DISPLAY-BIN-SIZE 2.0))
(def DISPLAY-BIN-SIZE-2   (/ DISPLAY-BIN-SIZE 2.0))
(def NEG-DISPLAY-BIN-SIZE (- DISPLAY-BIN-SIZE))
;;(def NUM-COL-BINS (/ DISPLAY-WIDTH  MOSAIC-BIN-SIZE))
;;(def NUM-ROW-BINS (/ DISPLAY-HEIGHT MOSAIC-BIN-SIZE))

(def CAM-BIN-SIZE (/ CAM-WIDTH NUM-COL-BINS))
(def CAM-BIN-SIZE-2 (/ CAM-BIN-SIZE 2.0))
(defn display->cam [display-x-or-y]
  (* display-x-or-y (/ CAM-WIDTH DISPLAY-WIDTH)))


(defn triangle-points
  "Saves origin points for each triangle, which is a vector pair of
   x and y positions in the image material [col row]."
  []
  (for [col-bin (range 0 NUM-COL-BINS)
        row-bin (range 0 NUM-ROW-BINS)
        :let [mat-col (* col-bin DISPLAY-BIN-SIZE)
              mat-row (* row-bin DISPLAY-BIN-SIZE)]
        :when (= 0 (mod col-bin 2))]
    [mat-col mat-row]))

(defn triangle-orientations
  "Radian orientations associated to triangle origin points."
  [triangle-points]
  (reduce (fn [memo [pt1 pt2]]
            (let [rot1 (rand-int 4)]
              (-> memo
                  (assoc-in [pt1] rot1))))
          {}
          (partition 2 triangle-points)))

(defn apex-top-right [x y]
  [(Point. x y)
   (Point. (+ x DISPLAY-BIN-SIZE-X2) y)
   (Point. (+ x DISPLAY-BIN-SIZE-X2) (+ y DISPLAY-BIN-SIZE-X2))])

(defn apex-bottom-left [x y]
  [(Point. (+ x DISPLAY-BIN-SIZE-X2) (+ y DISPLAY-BIN-SIZE-X2))
   (Point. x (+ y DISPLAY-BIN-SIZE-X2))
   (Point. x y)])

(defn apex-top-left [x y]
  [(Point. x (+ y DISPLAY-BIN-SIZE-X2))
   (Point. x y)
   (Point. (+ x DISPLAY-BIN-SIZE-X2) y)])

(defn apex-bottom-right [x y]
  [(Point. (+ x DISPLAY-BIN-SIZE-X2) y)
   (Point. (+ x DISPLAY-BIN-SIZE-X2) (+ y DISPLAY-BIN-SIZE-X2))
   (Point. x (+ y DISPLAY-BIN-SIZE-X2))])

(defn glyph [n x y]
  (cond
   (= n 0)
   (apex-top-right x y)

   (= n 1)
   (apex-bottom-right x y)

   (= n 2)
   (apex-bottom-left x y)

   (= n 3)
   (apex-top-left x y)))

(defn triangle-glyphs [triangle-points triangle-orientations]
    (reduce (fn [memo [[x y :as pt1] pt2]]
              (let [rot1 (triangle-orientations pt1)
                    rot2 (mod (+ rot1 2) 4)]
              (-> memo
                  (assoc-in [pt1] (glyph rot1 x y))
                  (assoc-in [pt2] (glyph rot2 x y)))))
          {}
          (partition 2 triangle-points)))

(defn mat
  ([]
     (Mat.))
  ([h w t]
     (Mat. (int h) (int w) t)))

;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  (let [tri-pts (triangle-points)
        tri-orients (triangle-orientations tri-pts)
        tri-glyphs (triangle-glyphs tri-pts tri-orients)]
   {:b-array (byte-array PIX-CNT1)
    :i-array (int-array PIX-CNT2)
    :frame-mat (mat CAM-HEIGHT CAM-WIDTH CvType/CV_8UC3)
    :output-mat (mat DISPLAY-HEIGHT DISPLAY-WIDTH CvType/CV_8UC4)
    :gray-mat (mat)
    :rgba-mat (mat CAM-HEIGHT CAM-WIDTH CvType/CV_8UC4)
    :drawn-mat (mat DISPLAY-HEIGHT DISPLAY-WIDTH CvType/CV_8UC4)
    :camera (cv/camera CAM-DEV-NUM)
    :p-image (q/create-image DISPLAY-WIDTH DISPLAY-HEIGHT :rgb)
    :triangle-points tri-pts
    :triangle-orientations tri-orients
    :triangle-glyphs tri-glyphs}))

(defn update-rgba [{:keys [rgba-mat frame-mat] :as state}]
  (assoc-in state [:rgba-mat] (cv/BGR->RGBA! frame-mat rgba-mat)))

(defn draw-mosaic-glyph [img-mat
                         color
                         glyph-pts]
  (let [poly (MatOfPoint.)
        c (apply (fn [r g b _]
                   (let [a ALPHA-STILL]
                     (Scalar. r g b a)))
                 color)]
    (.fromList poly (ArrayList. glyph-pts))
    (Core/fillPoly img-mat (ArrayList. [poly]) c)))

(defn draw-mosaic-pair
  [triangle-glyphs drawn-mat rgba-mat [pt1 pt2]]
  (let [color-fn (fn [display-x display-y]
                   (let [cam-x (display->cam display-x)
                         cam-y (display->cam display-y)
                         c (.get rgba-mat
                                 (+ cam-y CAM-BIN-SIZE-2)
                                 (+ cam-x CAM-BIN-SIZE-2))]
                     (if (< 0 (count c))
                       (vec c)
                       [0 0 0 255])))
        [display-x1 display-y1] pt1
        [display-x2 display-y2] pt2
        c1 (color-fn display-x1 display-y1)
        c2 (color-fn display-x2 display-y2)]
    (draw-mosaic-glyph drawn-mat c1 (triangle-glyphs pt1))
    (draw-mosaic-glyph drawn-mat c2 (triangle-glyphs pt2))))

(defn overlay-triangles
  [{:keys [drawn-mat rgba-mat triangle-points triangle-glyphs] :as state}]
  (dorun
   (map (partial draw-mosaic-pair
                 triangle-glyphs
                 drawn-mat
                 rgba-mat)
        (partition 2 triangle-points)))
  state)

(defn update-drawn-p-image [state]
  (let [{:keys [drawn-mat output-mat b-array i-array]} state]
    (update-in state [:p-image] #(cv/mat->p-img drawn-mat output-mat b-array i-array %))))

(defn update [state]
  (-> state
      (cv/update-frame)
      
      (update-rgba)
      (overlay-triangles)
      (update-drawn-p-image)))

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
