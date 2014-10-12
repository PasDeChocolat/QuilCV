(ns videotest.flow-only
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [clojure.set :as cset])
  (:import
   [org.opencv.highgui Highgui VideoCapture]
   [org.opencv.core CvType Mat MatOfByte MatOfFloat MatOfPoint MatOfPoint2f Size TermCriteria]
   [org.opencv.imgproc Imgproc]
   [org.opencv.video Video]
   [java.nio ByteBuffer ByteOrder]))

(def MAX-CORNERS 800)
(def BLANK-MASK (Mat.))
(def WIN-SIZE-W 10)
(def WIN-SIZE (Size. WIN-SIZE-W WIN-SIZE-W))
(def NEG-ONE-SIZE (Size. -1 -1))
(def TERM-CRITERIA (TermCriteria. (+ TermCriteria/EPS TermCriteria/COUNT) 20 0.03))

(def RESET-FRAMES 4)
(def PT-D 10)


;; OpenCV Implementation Wrapper - Will move to separate NS.
(defn camera [dev]
  (VideoCapture. dev))

(defn is-opened? [cam]
  (.isOpened cam))

(defn grab-frame!
  "Updates a CV Mat with image from camera."
  [cam frame]
  (.read cam frame)
  frame)

;; <--End OpenCV Implementation Wrapper


(defn camera-frame-size []
  (let [cam (camera 0)
        s (-> (grab-frame! cam (Mat.))
              (.size))]
    (.release cam)
    {:width (.width s) :height (.height s)}))

(def CAM-SIZE (camera-frame-size))
(def WIDTH  (int (:width  CAM-SIZE)))
(def HEIGHT (int (:height CAM-SIZE)))

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))


;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  {:b-array (byte-array PIX-CNT1)
   :i-array (int-array PIX-CNT2)
   :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC3)
   :output-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
   :old-gray-mat nil
   :old-corners nil
   :new-gray-mat (Mat.)
   :new-corners (MatOfPoint2f.)
   :lk-status (MatOfByte.)
   :lk-err (MatOfFloat.)
   :camera (camera 0)
   :p-image (q/create-image WIDTH HEIGHT :rgb)})

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

(defn update [state]
  (-> state
      (update-frame)
      #_(update-p-image)
      (init-gray-mat)
      (update-optical-flow)))

(defn draw-pt
  ([pt rgba]
    (let [x (.x pt)
          y (.y pt)]
       (q/push-style)
       (q/no-stroke)
       (apply q/fill rgba)
       (q/ellipse x y PT-D PT-D)
       (q/pop-style)) )
  ([pt]
     (draw-pt pt [0 255 0 80])))

(defn draw-flow-dots
  "Draw optical flow as dots."
  [{:keys [new-corners old-corners lk-status] :as state}]
  (let [pts (.toList new-corners)
        old-pts (.toList old-corners)]
    (when (< 0 (count old-pts))
      (dorun
       (map #(draw-pt % [0 0 255 60]) old-pts)))
    (when (< 0 (count pts))
      (let [v-pts (valid-pts pts (.toList lk-status))]
        (dorun
         (map draw-pt v-pts))))))

(defn draw-flow
  "Draw optical flow as lines."
  [{:keys [new-corners old-corners lk-status]}]
  (let [pts (.toList new-corners)
        old-pts (when old-corners
                  (.toList old-corners))]
    (when (< 0 (count old-pts))
      (dorun
       (map #(draw-pt % [0 0 255 60]) old-pts)))
    (when (< 0 (count pts))
      (let [v-pts (valid-pts pts (.toList lk-status))]
        (dorun
         (map draw-pt v-pts))))))

(defn draw [state]
  (let [{:keys [p-image]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate WIDTH 0)
    (q/scale -1 1)
    #_(q/image p-image 0 0)
    (draw-flow state)
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
