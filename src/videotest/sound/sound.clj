(ns videotest.sound.sound
  (:use [overtone.live])
  (:require [videotest.sound.harpsichord :as gharp]
            [overtone.inst.drum :as drum]
            [quil.core :as q]))


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
               max-len (q/map-range note NOTE_MIN NOTE_MAX MAX_BURST_LEN MIN_BURST_LEN)]]
     (swap! note-grid #(assoc % [col row] {:note note :played? played? :max-len max-len})))))

(defn harp
  [note duration]
  (gharp/play-single-note-by-int note duration))

(defn polyp-range []
  (doseq [note (range 100 106)]
    (harp note 200)
    (Thread/sleep 250)))

(def POLYP-CREATION-SOUND-NOTE-MIN 100)
(def POLYP-CREATION-SOUND-NOTE-MAX 105)
(def POLYP-CREATION-SOUND-DUR-MIN 0.1)
(def POLYP-CREATION-SOUND-DUR-MAX 1.0)

(defn harp-polyp-creation
  "Coral polyp creation sounds
  rel-note: Relative note, 0.0 to 1.0
  rel-duration: Relative duration, 0.0 1.0"
  [rel-note rel-duration]
  (let [note (int
              (q/map-range rel-note 0.0 1.0
                           POLYP-CREATION-SOUND-NOTE-MIN
                           POLYP-CREATION-SOUND-NOTE-MAX))
        duration (q/map-range rel-duration 0.0 1.0
                              POLYP-CREATION-SOUND-DUR-MIN
                              POLYP-CREATION-SOUND-DUR-MAX)]
    (harp note duration)))

(defn drum
  [freq]
  (drum/kick :amp 2.0 :freq freq :attack 0.01 :decay 0.5))

(defn bing [amp freq attack decay]
  (drum/bing :amp amp :freq freq :attack attack :decay decay))

(defn drum-polyp-creation [rel-freq]
  (let [freq (q/map-range rel-freq 0.0 1.0 300.0 200.0)]
    (drum freq)))

(defn bing-polyp-creation [rel-amp rel-freq]
  (let [amp (q/map-range rel-amp 0.0 1.0
                         0.1 0.5)
        freq (q/map-range rel-freq 0.0 1.0
                          700.0 520.0)]
    (bing amp freq 0.05 0.2)))

(defn polyp-decay-sound
  ([]
     (bing 2.0 220.0 0.05 0.2))
  ([rel-amp rel-freq]
     (let [amp (q/map-range rel-amp 0.0 1.0
                            0.5 2.0)
           freq (q/map-range rel-freq 0.0 1.0
                             120.0 520.0)
           attack 0.05
           decay 0.2]
       (bing amp freq attack decay))))


;; (defn hit-at-harpsichord
;;   [col row depth]
;;   (let [duration (qc/map-range depth DEPTH_START_SECOND_LAYER DEPTH_FAR_THRESH 0.5 20)
;;         rand-prob (rand)
;;         noise-row 22
;;         prob? (cond
;;                (and (>= (* NROWS 3/4) row) (> 0.37 rand-prob) ) true 
;;                (>= noise-row row) true
;;                (and (> row noise-row) (> 0.2 rand-prob)) true
;;                :else false)        
;;         {:keys [note played?]} (@note-grid [col row])]
;;     (when (and played? prob?)
;;       (gharp/play-single-note-by-int note duration))))


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
