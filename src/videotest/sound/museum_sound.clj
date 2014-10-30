(ns videotest.sound
  (:use [overtone.live])
  (:require [tutorial.harpsichord :as gharp]
            ;;[overtone.at-at :as at]
            [overtone.inst.drum :as drum]
            [quil.core :as qc]))

(def DEPTH_FAR_THRESH 4000.0)         ;; Museum setting 2
(def DEPTH_START_SECOND_LAYER 1000)
(def NCOLS 36)
(def NROWS 27)

;;(def at-at-pool (at/mk-pool))
;;(def long-col-state (atom {}))
(def note-grid (atom {}))

;;
;; Ideas:
;;  - If sector is commonly used, it could have it's volume degrade
;;  with use.

(defn hit-at-bing
  [col row depth]
  (let [
        d (qc/map-range depth 0 DEPTH_FAR_THRESH 0.0 1000.0)
        d (qc/constrain-float d 0.0 1000.0)
        
        ;; amp (qc/map-range d 0.0 1000.0 5.8 0.05)
        amp (qc/map-range row 0 NROWS 0.5 0.01)
        amp (qc/constrain-float amp 0.0 0.8)
        ;; amp 0.4

        freq (qc/map-range col 0 NCOLS 100.0 800.0)
        
        ;; attack (qc/map-range d 0.0 1000.0 1.0 0.01)
        ;; attack (qc/map-range row 0 NROWS 0.5 0.0001)
        attack (qc/map-range row 0 NROWS 0.0001 0.2)
        ;; attack 0.001

        ;; decay 0.1
        decay (qc/map-range d 0.0 1000.0 0.01 1.0)
        ;; decay (qc/map-range d 0.0 1000.0 10.0 0.1)
        decay (qc/constrain-float decay 0.1 1.0)
        ]
    (drum/bing :amp amp :freq freq :attack attack :decay decay)))

(comment
  ;; Create a note map to grid sectors.
  ;; lowest 60-24-9 = 27
  ;; highest 60+36+9 = 105

  (def NOTE_MIN 27)
  (def NOTE_MAX 105)
  (def MAX_BURST_LEN 60)
  (def MIN_BURST_LEN 10)

  (dorun
   (for [col (range NCOLS)
         row (range NROWS)
         :let [col-factor 2
               left-note (- 60 (int (/ NCOLS 2 col-factor)))
               the-note (int (+ (* (/ 1 col-factor) col) left-note))
               played? (= 0 (mod col col-factor))
               note (cond
                     (>= 2 row) (- the-note 24)
                     (>= 5 row) (- the-note 12)
                     (>= 10 row) the-note
                     (>= 15 row) (+ the-note 12)
                     (>= 20 row) (+ the-note 24)
                     (< 20 row) (+ the-note 36))
               max-len (qc/map-range note NOTE_MIN NOTE_MAX MAX_BURST_LEN MIN_BURST_LEN)]]
     (swap! note-grid #(assoc % [col row] {:note note :played? played? :max-len max-len})))))

(defn harp
  [note duration]
  (let [;;duration (qc/map-range depth DEPTH_START_SECOND_LAYER DEPTH_FAR_THRESH 0.5 20)
        ;; duration 20
        ;; rand-prob (rand)
        ;; noise-row 22
        ;; prob? (cond
        ;;        (and (>= (* NROWS 3/4) row) (> 0.37 rand-prob) ) true 
        ;;        (>= noise-row row) true
        ;;        (and (> row noise-row) (> 0.2 rand-prob)) true
        ;;        :else false)        
        ;;{:keys [note played?]} (@note-grid [col row])
        ]
    ;; (when (and played? prob?)
    ;;   (gharp/play-single-note-by-int note duration))
    (gharp/play-single-note-by-int note duration)))

(defn polyp-range []
  (doseq [note (range 100 106)]
    (harp note 200)
    (Thread/sleep 250)))

(defn polyp-creation []
  (harp 105 20))

(defn hit-at-harpsichord
  [col row depth]
  (let [duration (qc/map-range depth DEPTH_START_SECOND_LAYER DEPTH_FAR_THRESH 0.5 20)
        rand-prob (rand)
        noise-row 22
        prob? (cond
               (and (>= (* NROWS 3/4) row) (> 0.37 rand-prob) ) true 
               (>= noise-row row) true
               (and (> row noise-row) (> 0.2 rand-prob)) true
               :else false)        
        {:keys [note played?]} (@note-grid [col row])]
    (when (and played? prob?)
      (gharp/play-single-note-by-int note duration))))

(defn hit-at-harpsichordx
  [col row depth]
  (let [col-factor 2
        left-note (- 60 (int (/ NCOLS 2 col-factor)))
        the-note (int (+ (* (/ 1 col-factor) col) left-note))
        duration  (qc/map-range depth DEPTH_START_SECOND_LAYER DEPTH_FAR_THRESH 0.5 50)
        ;; duration  (qc/map-range depth DEPTH_START_SECOND_LAYER DEPTH_FAR_THRESH 100 1)
        rand-prob (rand)
        prob (> 0.37 rand-prob)]
    (when (and prob
           (= 0 (mod col col-factor)))
      (cond
       (>= 2 row) (gharp/play-single-note-by-int  (- the-note 24) duration)
       (>= 5 row) (gharp/play-single-note-by-int  (- the-note 12) duration)
       (>= 10 row) (gharp/play-single-note-by-int the-note duration)
       (>= 15 row) (gharp/play-single-note-by-int (+ the-note 12) duration)
       (>= 20 row) (gharp/play-single-note-by-int (+ the-note 24) duration)
       ;; (< 23 row) (gharp/play-single-note-by-int (+ the-note 36) duration)
       (and (> 0.2 rand-prob) (< 20 row)) (gharp/play-single-note-by-int (+ the-note 36) duration)
       ))))

(defn hit-at
  [col row depth]
  ;; (hit-at-bing col row depth)
  (hit-at-harpsichord col row depth))

;; Binging
;; (defn bing-with
;;   [freq]
;;   (drum/bing :amp 2.0 :freq freq :attack 0.01 :decay 0.5))

;; (defn rhythm-hit-at-dispatch [col row depth]
;;   :bing)

;; (defmulti rhythm-hit-at
;;   "Play the rhythm part of this thing."
;;   #'rhythm-hit-at-dispatch
;;   :default :bing)

;; (defmethod rhythm-hit-at :bing [col row depth]
;;   (let [freq (qc/map-range col 0 (last LONG_COLS_START_COLS) 120.0 180.0)
;;         timing [0 400 800 1600 400 800 400]
;;         last-index (dec (count timing))
;;         do-bing (fn [index]
;;                   (if (= index last-index)
;;                     (swap! long-col-state #(assoc % col false)))
;;                   (bing-with freq)
;;                   )]
;;     ;; (drum/bing :amp 2.0 :freq freq :attack 0.01 :decay 0.5)
;;     (dorun (map-indexed #(at/at (+ (at/now) %2) (do-bing %1) at-at-pool) timing))
;;     ))

;; (defmethod rhythm-hit-at :kick [col row depth]
;;   (let [freq (qc/map-range col 0 (last LONG_COLS_START_COLS) 200.0 250.0)]
;;     (drum/kick :amp 0.00001 :freq freq :attack 0.01 :decay 0.5)))

;; (drum/bing :amp 2.0 :freq 120.0 :attack 0.01 :decay 0.5)
;; (drum/kick :amp 2.0 :freq 200.0 :attack 0.01 :decay 0.5)
;; (drum/dance-kick)
;; (dorun (map #(at (+ (now) %) (drum/bing :amp 2.0 :freq 180)) [0 200 400 800 200 400 200]))
