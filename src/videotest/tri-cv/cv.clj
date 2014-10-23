(ns videotest.triangles.cv
  (:require
   [quil.core :as q]
   [clojure.set :as cset]
   [clojure.math.numeric-tower :as math])
  (:import
   [org.opencv.highgui Highgui VideoCapture]
   [org.opencv.core CvType Mat MatOfPoint MatOfPoint2f Size TermCriteria]
   [org.opencv.imgproc Imgproc]
   [org.opencv.video Video]
   [java.nio ByteBuffer ByteOrder]))

(def MAX-CORNERS 600)
(def BLANK-MASK (Mat.))
(def WIN-SIZE-W 10)
(def WIN-SIZE (Size. WIN-SIZE-W WIN-SIZE-W))
(def NEG-ONE-SIZE (Size. -1 -1))
(def TERM-CRITERIA (TermCriteria. (+ TermCriteria/EPS TermCriteria/COUNT) 20 0.03))

(def RESET-FRAMES 4)
(def FLOW-MAX 50)
(def FLOW-MIN 10)

;; OpenCV Implementation Wrapper - Will move to separate NS.
(defn camera [dev]
  (VideoCapture. dev))

(defn is-opened? [cam]
  (and (not (nil? cam))
       (.isOpened cam)))

(defn grab-frame!
  "Updates a CV Mat with image from camera."
  [cam frame]
  (.read cam frame)
  frame)

(defn mat-empty? [m]
  (if (nil? m)
    true
    (= 0.0 (.area (.size m)))))

;; <--End OpenCV Implementation Wrapper


(defn camera-frame-size []
  (let [cam (camera 0)
        s (-> (grab-frame! cam (Mat.))
              (.size))]
    (.release cam)
    {:width (.width s) :height (.height s)}))


(defn update-frame [state]
  (let [{:keys [camera]} state]
    (cond-> state
            (is-opened? camera)
            (update-in [:frame-mat] (partial grab-frame! camera) ))))


;; Processing PImage conversion
(defn any-mat->p-img
  "See Imgproc/cvtColor for list of cvt-color-codes."
  [in-mat out-mat cvt-color-code b-array i-array p-img]
  (Imgproc/cvtColor in-mat out-mat cvt-color-code 4)
  (.get out-mat 0 0 b-array)
  (-> (ByteBuffer/wrap b-array)
      (.order ByteOrder/LITTLE_ENDIAN)
      (.asIntBuffer)
      (.get i-array))
  (.loadPixels p-img)
  (set! (.pixels p-img) (aclone i-array))
  (.updatePixels p-img)
  p-img)

(defn gray-mat->p-img [in-mat out-mat b-array i-array p-img]
  (any-mat->p-img in-mat out-mat Imgproc/COLOR_GRAY2RGBA b-array i-array p-img))

(defn mat->p-img
  "Input mat is really BGR, but we'll flip these bits in
   any-mat->p-img. If you don't use that, swap blue and red."
  [in-mat out-mat b-array i-array p-img]
  (any-mat->p-img in-mat out-mat Imgproc/COLOR_RGB2RGBA b-array i-array p-img))

(defn update-p-image [state]
  (let [{:keys [frame-mat output-mat b-array i-array]} state]
    (update-in state [:p-image] #(mat->p-img frame-mat output-mat b-array i-array %))))


;; Camera frame to gray
(defn BGR->GrayMat! [m gray-mat]
  (Imgproc/cvtColor m gray-mat Imgproc/COLOR_BGR2GRAY)
  gray-mat)

(defn BGR->RGBA! [m rgba-mat]
  (Imgproc/cvtColor m rgba-mat Imgproc/COLOR_BGR2RGBA)
  rgba-mat)


;; Filtering
(defn gray-mat->bw-mat
  ([gray-mat bw-mat]
     (gray-mat->bw-mat gray-mat bw-mat {}))
  ([gray-mat bw-mat {:keys [bw-threshold] :or {bw-threshold 126}}]
     (Imgproc/threshold gray-mat bw-mat bw-threshold 255 Imgproc/THRESH_BINARY)
     bw-mat))

(defn gray-mat->inv-bw-mat
  ([gray-mat bw-mat]
     (gray-mat->inv-bw-mat gray-mat bw-mat {}))
  ([gray-mat bw-mat {:keys [bw-threshold] :or {bw-threshold 126}}]
     (Imgproc/threshold gray-mat bw-mat bw-threshold 255 Imgproc/THRESH_BINARY_INV)
     bw-mat))

(defn median-blur
  [src-mat dst-mat k-size]
  (Imgproc/medianBlur src-mat dst-mat k-size)
  dst-mat)

(defn gray-mat->adaptive-mean-bw
  "I can't even believe how slow this is. Use at own risk."
  ([gray-mat bw-mat]
     (gray-mat->adaptive-mean-bw gray-mat bw-mat {}))
  ([gray-mat bw-mat {:keys [threshold-type block-size c blur-k-size]
                     :or {threshold-type Imgproc/THRESH_BINARY
                          block-size 11
                          c 2
                          blur-k-size 5}}]
     (let [src-mat (median-blur gray-mat bw-mat blur-k-size)]
      (Imgproc/adaptiveThreshold src-mat bw-mat
                                 255
                                 Imgproc/ADAPTIVE_THRESH_MEAN_C
                                 threshold-type
                                 block-size c))
     bw-mat))

(defn gray-mat->adaptive-mean-inv-bw
  "I can't even believe how slow this is. Use at own risk."
  ([gray-mat bw-mat]
     (gray-mat->adaptive-mean-inv-bw gray-mat bw-mat {}))
  ([gray-mat bw-mat {:keys [threshold-type blur-k-size]
                     :or {threshold-type Imgproc/THRESH_BINARY_INV
                          blur-k-size 5}
                     :as opts}]
     (let [opts (assoc-in opts [:threshold-type] threshold-type)
           src-mat (median-blur gray-mat bw-mat blur-k-size)]
       (gray-mat->adaptive-mean-inv-bw src-mat bw-mat opts))))

(defn gray-mat->adaptive-gaussian-bw
  "I can't even believe how slow this is. Use at own risk."
  ([gray-mat bw-mat]
     (gray-mat->adaptive-gaussian-bw gray-mat bw-mat {}))
  ([gray-mat bw-mat {:keys [threshold-type block-size c]
                     :or {threshold-type Imgproc/THRESH_BINARY
                          block-size 11
                          c 2}}]
     (Imgproc/adaptiveThreshold gray-mat bw-mat
                                255
                                Imgproc/ADAPTIVE_THRESH_GAUSSIAN_C
                                threshold-type
                                block-size c)
     bw-mat))

(defn gaussian-blur
  ([src-mat dst-mat]
     (gaussian-blur src-mat dst-mat {}))
  ([src-mat dst-mat {:keys [k-size sigma-x]
                     :or {k-size (Size. 5 5)
                          sigma-x 0}}]
     (Imgproc/GaussianBlur src-mat dst-mat k-size sigma-x)
     dst-mat))

(defn gray-mat->otsu-gaussian-bw
  ([gray-mat bw-mat gauss-mat]
     (gray-mat->otsu-gaussian-bw gray-mat bw-mat gauss-mat {}))
  ([gray-mat bw-mat gauss-mat {:keys [threshold-type blur-k-size]
                               :or {threshold-type Imgproc/THRESH_BINARY
                                    blur-k-size (Size. 5 5)}}]
     (let [src-mat (gaussian-blur gray-mat gauss-mat {:k-size blur-k-size})]
       (Imgproc/threshold src-mat bw-mat 0 255 (+ threshold-type
                                                  Imgproc/THRESH_OTSU))
       bw-mat)))


;; Feature detection (for optical flow)
(defn update-corners!
  ([gray-mat]
     (let [corners (MatOfPoint2f.)]
       (update-corners! gray-mat corners)))
  ([gray-mat corners]
     (let [max-corners MAX-CORNERS
           quality-level 0.01
           min-dist 5.0
           mask BLANK-MASK
           block-size 3
           use-harris-detector false
           k 0.04
           pts (MatOfPoint.)]
       (Imgproc/goodFeaturesToTrack gray-mat pts max-corners quality-level min-dist mask block-size use-harris-detector k)
       (.convertTo pts corners CvType/CV_32FC2)
       #_(Imgproc/cornerSubPix gray-mat corners WIN-SIZE NEG-ONE-SIZE TERM-CRITERIA)
       corners)))

(defn valid-pts [pts pt-status]
  (let [p-s (filter (fn [[_ s]] (= 1 s))
                    (map vector pts pt-status))]
    (doall
     (map (fn [[p _]] p) p-s))))

(defn refresh-corners! [gray-mat lk-status remaining-corners old-corners]
  (let [valid-remaining (valid-pts (.toList remaining-corners)
                                   (.toList lk-status))
        new-corners (.toList (update-corners! gray-mat))
        union (vec
               (cset/union (set valid-remaining) (set new-corners)))]
    (.fromList old-corners (take MAX-CORNERS union))))

(defn init-gray-mat [{:keys [frame-mat old-gray-mat] :as state}]
  (if (nil? old-gray-mat)
    (let [old-gray-mat (BGR->GrayMat! frame-mat (Mat.))]
      (-> state
          (assoc-in [:old-gray-mat] old-gray-mat)
          (assoc-in [:old-corners] (update-corners! old-gray-mat (MatOfPoint2f.)))))
    (let [{:keys [old-gray-mat old-corners new-gray-mat new-corners lk-status]} state]
      (.copyTo new-gray-mat old-gray-mat)
      (if (= 0 (mod (q/frame-count) RESET-FRAMES))
        (do
          ;; Reuse existing valid corners (still detected):
          (refresh-corners! new-gray-mat lk-status new-corners old-corners)

          ;; Brute-Force reset of corners:
          #_(update-corners! new-gray-mat old-corners))
        (do
          (.copyTo new-corners old-corners)))
      state))) 

(defn update-optical-flow [state]
  (let [{:keys [frame-mat new-gray-mat new-corners old-gray-mat old-corners lk-status lk-err]} state]
    (BGR->GrayMat! frame-mat new-gray-mat)
    (update-corners! new-gray-mat new-corners)
    (let [max-level 5
          flags 0
          min-eig-threshold 0.001]
      (Video/calcOpticalFlowPyrLK old-gray-mat new-gray-mat old-corners new-corners lk-status lk-err
                                  WIN-SIZE max-level TERM-CRITERIA flags min-eig-threshold))
    (-> state
        (assoc-in [:lk-status] lk-status)
        (assoc-in [:lk-err] lk-err))))

(defn- sqr [n]
  (math/expt n 2))

(defn small-flow [[old-pt new-pt]]
  "Filter predicate for flow-pts, only selecting flow points
   less than FLOW-MAX away from each other."
  (let [old-x (.x old-pt)
        old-y (.y old-pt)
        new-x (.x new-pt)
        new-y (.y new-pt)
        d-x (- new-x old-x)
        d-y (- new-y old-y)
        d-sq (+ (* d-x d-x) (* d-y d-y))
        lim-sq (sqr FLOW-MAX)]
    (>= lim-sq d-sq)))

(defn trim-flow [[old-pt new-pt]]
  "Filter predicate for flow-pts, only selecting flow points
   less than FLOW-MAX away from each other."
  (let [old-x (.x old-pt)
        old-y (.y old-pt)
        new-x (.x new-pt)
        new-y (.y new-pt)
        d-x (- new-x old-x)
        d-y (- new-y old-y)
        d-sq (+ (* d-x d-x) (* d-y d-y))
        max-sq (sqr FLOW-MAX)
        min-sq (sqr FLOW-MIN)]
    (and (<= min-sq d-sq) (>= max-sq d-sq))))

(defn record-flow-pts
  "Records flow points [old-pt new-pt]."
  [{:keys [new-corners old-corners lk-status] :as state}]
  (let [status (if (mat-empty? lk-status)
                 []
                 (.toList lk-status))
        pts (if new-corners
              (valid-pts (.toList new-corners) status)
              [])
        old-pts (if old-corners
                  (valid-pts (.toList old-corners) status)
                  [])
        flow-filter trim-flow
        ; flow-filter small-flow
        flow-pts (filter flow-filter
                         (map vector old-pts pts))]
    (-> state
        (assoc-in [:flow-pts] flow-pts))))

(defn update-image-detection [state]
  (-> state
      (update-frame)
      #_(update-p-image)
      (init-gray-mat)
      (update-optical-flow)
      (record-flow-pts)))
