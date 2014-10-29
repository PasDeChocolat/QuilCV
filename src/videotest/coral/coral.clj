(ns videotest.coral.coral
  (:require
   [quil.core :as q]
   [videotest.coral.color :as color]
   [videotest.coral.hex :as hex]))


;; Should have a chance to attach, when:
;; - at bottom,
;; - run into existing coral,
;; - 

;; (def NUM-CORAL-COL-BINS 64.0)
;; (def HEX-W 10.0)
(def NUM-CORAL-COL-BINS 110.0)
(def HEX-W 7.6)

(defn coral-size [display-w display-h]
  (let [col-bins NUM-CORAL-COL-BINS
        cell-w (/ display-w col-bins)
        row-bins (/ display-h cell-w)
        hex-w HEX-W]
    {:num-col-bins col-bins
     :num-row-bins row-bins
     :cell-w cell-w
     :cell-half-w (/ cell-w 2.0)
     :hex-w hex-w
     :hex-half-w (/ hex-w 2.0)
     :hex-y-offset (hex/hex-y-offset hex-w)}))

(defn x->col [cell-w x]
  (int (/ x cell-w)))

(defn y->row [cell-w y]
  (int (/ y cell-w)))

(defn xy->coords
  ([cell-w [x y]]
     (xy->coords cell-w x y))
  ([cell-w x y]
     (let [col (x->col cell-w x)
           row (y->row cell-w y)]
       [col row])))

(defn is-bottom? [num-rows row]
  (<= (dec num-rows) row))

(defn is-occupied? [coral coords]
  (contains? coral coords))

(defn occupied-coral-under [coral [col row]]
  (let [row-under (inc row)
        under-coords (if (even? col)
                       [[(dec col) row]
                        [     col  row-under]
                        [(inc col) row]]
                       [[(dec col) row-under]
                        [     col  row-under]
                        [(inc col) row-under]])
        ]
    (filter #(is-occupied? coral %) under-coords)))

(defn is-row-under-occupied? [coral [col row]]
  (seq (occupied-coral-under coral [col row])))

(def BOTTOM-ATTACH-PCT 0.1)

;; 127.5 = 180 degress (max on 0-255 scale)
;; 21.25 = 30 degrees (on 0-255 scale)
(def HUE-DIFF-CLIQUEY-THRESH-MAX 21.25)

(defn is-cliquey? [coral coords this-hue]
  (if-let [occupied-under (seq
                           (occupied-coral-under coral coords))]
    (let [sum-hue (reduce (fn [memo under-coords]
                            (let [{:keys [color-hsva]} (coral under-coords)]
                              (+ memo (first color-hsva))))
                          0
                          occupied-under)
          avg-hue (/ sum-hue (count occupied-under))
          diff (color/hue-diff avg-hue this-hue)]
      (and (< diff HUE-DIFF-CLIQUEY-THRESH-MAX) 
           (> (q/map-range diff
                           0 HUE-DIFF-CLIQUEY-THRESH-MAX
                           1.0 0.0) (rand))))
    false))

(defn is-attaching? [num-rows cell-w coral
                     {:keys [x y color-hsva] :as seed}]
  (let [[col row :as coords] (xy->coords cell-w x y)]
    (and (not (is-occupied? coral coords))
         (not (is-occupied? coral [col (dec row)]))
         (or (and (is-bottom? num-rows row)
                  (> BOTTOM-ATTACH-PCT (rand)))
             (is-cliquey? coral coords (first color-hsva)))
    )))

(defn remove-seeds [all-seeds seeds]
  (remove (set seeds) all-seeds))


(defn add-polyp [cell-w coral x y color-rgba color-hsva]
  (let [[col row :as coords] (xy->coords cell-w x y)]
    (assoc-in coral [coords] {:color-rgba color-rgba
                              :color-hsva color-hsva})))

(defn add-seeds-to-coral [cell-w coral seeds]
  (doall
   (reduce (fn [memo {:keys [x y color-rgba color-hsva] :as seed}]
             (add-polyp cell-w memo
                        x y
                        color-rgba color-hsva))
           coral
           seeds)))

(defn attach-seeds
  [display-h {:keys [motion-seeds coral-size coral] :as state}]
  (let [{:keys [num-row-bins cell-w]} coral-size
        attaching (filter (fn [seed]
                            (is-attaching? num-row-bins cell-w coral seed))
                          motion-seeds)]
    (-> state
        (update-in [:motion-seeds] #(remove-seeds % attaching))
        (update-in [:coral] #(add-seeds-to-coral cell-w % attaching)))))

(def CORAL-ROT-CYCLE 120)

(defn rotate-coral [state]
  (if (= 0 (mod (q/frame-count) CORAL-ROT-CYCLE))
    (update-in state [:coral]
              #(reduce (fn [memo [[col row :as coords]
                                 polyp]]
                         (if (< col 1)
                           memo
                           (let [new-coords [(dec col) row]]
                             (assoc-in memo [new-coords] polyp))))
                       {}
                       %))
    state))

(defn draw-polyp [odd-col-lower
                  {:keys [cell-w cell-half-w
                          hex-w hex-half-w
                          hex-y-offset] :as coral-size}
                  [[col row :as coords]
                   {:keys [color-rgba] :as polyp}]]
  #_(apply q/fill color-rgba)
  (apply q/stroke color-rgba)
  (hex/draw-hex-cell odd-col-lower
                     cell-w cell-half-w
                     hex-w hex-half-w
                     hex-y-offset
                     col (- row 1)))

(defn draw-coral [{:keys [coral-size coral]}]
  (q/push-style)
  (q/no-fill)
  (q/stroke-weight 4.0)
  #_(q/stroke 255)
  (let [cell-w (:cell-w coral-size)
        cycle (mod (q/frame-count) CORAL-ROT-CYCLE)
        x-offset (* (/ cell-w (float CORAL-ROT-CYCLE))
                    (float cycle))
        cycle2 (mod (q/frame-count) (* 2 CORAL-ROT-CYCLE))
        odd-col-lower (>= cycle2 CORAL-ROT-CYCLE)]
    (q/with-translation [(- x-offset) 0]
      (dorun
       (map (partial draw-polyp odd-col-lower coral-size)
            coral))))
  (q/pop-style))
