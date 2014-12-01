(ns videotest.optical-flow-lk
  (:require
   [quil.core :as q]
   [quil.middleware :as m])
  (:import
   [org.opencv.highgui Highgui VideoCapture]
   [org.opencv.core CvType Mat MatOfByte MatOfFloat MatOfPoint MatOfPoint2f]
   [org.opencv.imgproc Imgproc]
   [org.opencv.video Video]
   [java.nio ByteBuffer ByteOrder]))

(def WIDTH 640)
(def HEIGHT 480)

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))


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


;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  #_(q/frame-rate 60)
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

(defn update-corners! [gray-mat corners]
  (let [max-corners 100
        quality-level 0.3
        min-dist 7
        pts (MatOfPoint.)]
    (Imgproc/goodFeaturesToTrack gray-mat pts max-corners quality-level min-dist)
    (.convertTo pts corners CvType/CV_32FC2)
    corners))

(defn init-gray-mat [{:keys [frame-mat old-gray-mat] :as state}]
  (if (nil? old-gray-mat)
    (let [old-gray-mat (BGR->GrayMat! frame-mat (Mat.))]
      (-> state
          (assoc-in [:old-gray-mat] old-gray-mat)
          (assoc-in [:old-corners] (update-corners! old-gray-mat (MatOfPoint2f.)))))
    (let [{:keys [old-gray-mat old-corners new-gray-mat new-corners]} state]
      (.copyTo new-gray-mat old-gray-mat)
      (.copyTo new-corners old-corners)
      (-> state
          (assoc-in [:old-gray-mat] old-gray-mat)
          (assoc-in [:old-corners] old-corners)))))

(defn update-optical-flow [state]
  (let [{:keys [frame-mat new-gray-mat new-corners old-gray-mat old-corners lk-status lk-err]} state]
    (BGR->GrayMat! frame-mat new-gray-mat)
    (update-corners! new-gray-mat new-corners)
    (Video/calcOpticalFlowPyrLK old-gray-mat new-gray-mat old-corners new-corners lk-status lk-err)
    (-> state
        (assoc-in [:new-gray-mat] new-gray-mat)
        (assoc-in [:new-corners] new-corners)
        (assoc-in [:lk-status] lk-status)
        (assoc-in [:lk-err] lk-err))))

(defn update [state]
  (-> state
      (update-frame)
      (update-p-image)
      (init-gray-mat)
      (update-optical-flow)))

(defn draw-pt [pt]
  (let [x (.x pt)
        y (.y pt)]
    (q/push-style)
    (q/no-stroke)
    (q/fill 0 255 0 150)
    (q/ellipse x y 20 20)
    (q/pop-style)))

(defn draw-flow [{:keys [new-corners lk-status] :as state}]
  (let [pts (.toList new-corners)]
    (when (< 0 (count pts))
      (let [status (.toList lk-status)
            z (filter (fn [[_ s]] (= 1 s))
                      (map vector pts status))]
        (dorun
         (map (fn [[p _]] (draw-pt p))
              z))))))

(defn draw [state]
  (let [{:keys [p-image]} state]
    (q/background 0)
    (q/image p-image 0 0)
    (draw-flow state)))

(defn on-close
  ([{:keys [camera]}]
     (println "closing sketch and releasing camera...")
     (when-not (nil? camera)
       (.release camera))))

(q/defsketch videotest
  :title "Video Test"
  :size [WIDTH HEIGHT]
  :setup setup
  ;; something for on-close??? (.release camera)
  :update update
  :draw draw
  :on-close on-close
  :middleware [m/fun-mode])
