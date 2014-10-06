(ns videotest.buffers
  (:require
   [quil.core :as q]
   [quil.middleware :as m])
  (:import
   [org.opencv.highgui Highgui VideoCapture]
   [org.opencv.core CvType Mat]
   [org.opencv.imgproc Imgproc]
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
  (q/frame-rate 60)
  {:b-array (byte-array PIX-CNT1)
   :i-array (int-array PIX-CNT2)
   :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
   :camera (camera 0)
   :p-image (q/create-image WIDTH HEIGHT :argb)})

(defn update-frame [state]
  (let [{:keys [camera]} state]
    (cond-> state
            (is-opened? camera)
            (update-in [:frame-mat] (partial grab-frame! camera) ))))

(defn mat->p-img [mat b-array i-array p-img]
  (let [n-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)]
    (Imgproc/cvtColor mat n-mat Imgproc/COLOR_BGRA2RGBA 0)
    (.get n-mat 0 0 b-array)
    (-> (ByteBuffer/wrap b-array)
       (.asIntBuffer)
       (.get i-array)))
  (.loadPixels p-img)
  (set! (.pixels p-img) (aclone i-array))
  (.updatePixels p-img)
  p-img)

(defn update-p-image [state]
  (let [{:keys [frame-mat b-array i-array]} state]
    (update-in state [:p-image] #(mat->p-img frame-mat b-array i-array %))))

(defn update [state]
  (-> state
      (update-frame)
      (update-p-image)))

(defn draw [state]
  (let [{:keys [p-image]} state]
    (q/background 0)
    (q/image p-image 0 0)))

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
