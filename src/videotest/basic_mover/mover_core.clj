(ns videotest.basic-mover.mover-core
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [videotest.basic-mover.cv :as cv]
   [videotest.basic-mover.flock :as flock])
  (:import
   [org.opencv.core CvType Mat MatOfByte MatOfFloat MatOfPoint MatOfPoint2f]))


(def CAM-SIZE (cv/camera-frame-size))
(def WIDTH  (int (:width  CAM-SIZE)))
(def HEIGHT (int (:height CAM-SIZE)))

;; pixCnt1 is the number of bytes in the pixel buffer
;; pixCnt2 is the number of integers in the PImage pixels buffer
(def PIX-CNT1 (* WIDTH HEIGHT 4))
(def PIX-CNT2 (* WIDTH HEIGHT))

(def FLOW-MAX 50)

(def NUM-VEHICLES 5)

;; bArray is the temporary byte array buffer for OpenCV cv::Mat.
;; iArray is the temporary integer array buffer for PImage pixels.
(defn setup []
  (q/frame-rate 60)
  {:vehicles (flock/init-vehicles WIDTH HEIGHT NUM-VEHICLES)
   :b-array (byte-array PIX-CNT1)
   :i-array (int-array PIX-CNT2)
   :frame-mat (Mat. WIDTH HEIGHT CvType/CV_8UC3)
   :output-mat (Mat. WIDTH HEIGHT CvType/CV_8UC4)
   :old-gray-mat nil
   :old-corners nil
   :new-gray-mat (Mat.)
   :new-corners (MatOfPoint2f.)
   :lk-status (MatOfByte.)
   :lk-err (MatOfFloat.)
   :camera (cv/camera 0)
   :p-image (q/create-image WIDTH HEIGHT :rgb)})

(defn update [state]
  (-> state
      (cv/update-frame)
      #_(cv/update-p-image)
      (cv/init-gray-mat)
      (cv/update-optical-flow)
      (update-in [:vehicles] (partial flock/update-vehicles WIDTH HEIGHT))
      ))

(defn draw-line [old-pt new-pt]
  (q/line (.x old-pt) (.y old-pt)
          (.x new-pt) (.y new-pt)))

(defn small-flow [[old-pt new-pt]]
  (let [old-x (.x old-pt)
        old-y (.y old-pt)
        new-x (.x new-pt)
        new-y (.y new-pt)
        d-x (- new-x old-x)
        d-y (- new-y old-y)
        d-sq (+ (* d-x d-x) (* d-y d-y))
        lim-sq (* FLOW-MAX FLOW-MAX)]
    (>= lim-sq d-sq)))

(defn draw-flow
  "Draw optical flow as lines."
  [{:keys [new-corners old-corners lk-status]}]
  (let [status (if (cv/mat-empty? lk-status)
                 []
                 (.toList lk-status))
        pts (when new-corners
              (cv/valid-pts (.toList new-corners) status))
        old-pts (when old-corners
                  (cv/valid-pts (.toList old-corners) status))]
    (when (< 0 (count pts))
      (let [valid-pts (filter small-flow
                              (map vector old-pts pts))]
        (q/push-matrix)
        (q/stroke 255)
        (q/stroke-weight 0.5)
        (dorun
         (map (fn [[o n]]
                (draw-line o n))
              valid-pts))
        (q/pop-matrix)))))

(defn draw [state]
  (let [{:keys [p-image vehicles]} state]
    (q/background 0)
    (q/push-matrix)
    (q/translate WIDTH 0)
    (q/scale -1 1)
    #_(q/image p-image 0 0)
    (draw-flow state)
    (doall (map flock/draw-vehicle vehicles))
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
