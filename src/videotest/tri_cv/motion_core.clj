(ns videotest.tri-cv.motion-core
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
(def WIDTH  (int (:width  CAM-SIZE)))
(def HEIGHT (int (:height CAM-SIZE)))

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))

(def ALPHA-STILL 255.0)

(def MOSAIC-BIN-SIZE 36)
(def MOSAIC-BIN-SIZE-X2  (* MOSAIC-BIN-SIZE 2.0))
(def MOSAIC-BIN-SIZE-2   (/ MOSAIC-BIN-SIZE 2.0))
(def NEG-MOSAIC-BIN-SIZE (- MOSAIC-BIN-SIZE))
(def NUM-COL-BINS (/ WIDTH  MOSAIC-BIN-SIZE))
(def NUM-ROW-BINS (/ HEIGHT MOSAIC-BIN-SIZE))

(defn triangle-points
  "Saves origin points for each triangle, which is a vector pair of
   x and y positions in the image material [col row]."
  []
  (for [col-bin (range 0 NUM-COL-BINS)
        row-bin (range 0 NUM-ROW-BINS)
        :let [mat-col (* col-bin MOSAIC-BIN-SIZE)
              mat-row (* row-bin MOSAIC-BIN-SIZE)]
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
   (Point. (+ x MOSAIC-BIN-SIZE-X2) y)
   (Point. (+ x MOSAIC-BIN-SIZE-X2) (+ y MOSAIC-BIN-SIZE-X2))])

(defn apex-bottom-left [x y]
  [(Point. (+ x MOSAIC-BIN-SIZE-X2) (+ y MOSAIC-BIN-SIZE-X2))
   (Point. x (+ y MOSAIC-BIN-SIZE-X2))
   (Point. x y)])

(defn apex-top-left [x y]
  [(Point. x (+ y MOSAIC-BIN-SIZE-X2))
   (Point. x y)
   (Point. (+ x MOSAIC-BIN-SIZE-X2) y)])

(defn apex-bottom-right [x y]
  [(Point. (+ x MOSAIC-BIN-SIZE-X2) y)
   (Point. (+ x MOSAIC-BIN-SIZE-X2) (+ y MOSAIC-BIN-SIZE-X2))
   (Point. x (+ y MOSAIC-BIN-SIZE-X2))])

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

;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  (let [tri-pts (triangle-points)
        tri-orients (triangle-orientations tri-pts)
        tri-glyphs (triangle-glyphs tri-pts tri-orients)]
   {:b-array (byte-array PIX-CNT1)
    :i-array (int-array PIX-CNT2)
    :frame-mat (Mat. HEIGHT WIDTH CvType/CV_8UC3)
    :output-mat (Mat. HEIGHT WIDTH CvType/CV_8UC4)
    :gray-mat (Mat.)
    :rgba-mat (Mat.)
    :drawn-mat (Mat. HEIGHT WIDTH CvType/CV_8UC4)
    :camera (cv/camera CAM-DEV-NUM)
    :p-image (q/create-image WIDTH HEIGHT :rgb)
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
  (let [color-fn (fn [mat-col mat-row]
                   (let [c (.get rgba-mat
                                 (+ mat-row MOSAIC-BIN-SIZE-2)
                                 (+ mat-col MOSAIC-BIN-SIZE-2))]
                     (if (< 0 (count c))
                       (vec c)
                       [0 0 0])))
        [mat-col1 mat-row1] pt1
        [mat-col2 mat-row2] pt2
        c1 (color-fn mat-col1 mat-row1)
        c2 (color-fn mat-col2 mat-row2)]
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

(defn rgba-mat->p-img
  "Input mat is really BGR, but we'll flip these bits in
   any-mat->p-img. If you don't use that, swap blue and red."
  [in-mat out-mat b-array i-array p-img]
  (cv/any-mat->p-img in-mat out-mat Imgproc/COLOR_RGBA2BGRA b-array i-array p-img))

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
    (q/translate WIDTH 0)
    (q/scale -1 1)
    (when p-image
      (q/image p-image 0 0)
      #_(draw-centroids state))
    #_(when-not (cv/mat-empty? frame-mat)
      (draw-mosaic state))
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
