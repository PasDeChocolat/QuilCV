(ns videotest.basic-mover.cv
  (:require
   [quil.core :as q]
   [clojure.set :as cset])
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

(defn mat->p-img [in-mat out-mat b-array i-array p-img]
  (Imgproc/cvtColor in-mat out-mat Imgproc/COLOR_RGB2RGBA 4)
  (.get out-mat 0 0 b-array)
  (-> (ByteBuffer/wrap b-array)
      (.order ByteOrder/LITTLE_ENDIAN)
      (.asIntBuffer)
      (.get i-array))
  (.loadPixels p-img)
  (set! (.pixels p-img) (aclone i-array))
  (.updatePixels p-img)
  p-img)

(defn update-p-image [state]
  (let [{:keys [frame-mat output-mat b-array i-array]} state]
    (update-in state [:p-image] #(mat->p-img frame-mat output-mat b-array i-array %))))

(defn BGR->GrayMat! [m gray-mat]
  (Imgproc/cvtColor m gray-mat Imgproc/COLOR_BGR2GRAY)
  gray-mat)

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
