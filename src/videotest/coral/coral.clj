(ns videotest.coral.coral
  (:require
   [quil.core :as q]
   [videotest.coral.hex :as hex]))


;; Should have a chance to attach, when:
;; - at bottom,
;; - run into existing coral,
;; - 

(def NUM-CORAL-COL-BINS 64.0)
(def HEX-W 10.0)

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

(defn is-occupied? [x y]
  )

(defn bumping-coral? []
  )

(defn is-attaching? [num-rows cell-w x y]
  (is-bottom? num-rows (y->row cell-w y)))

(defn remove-seeds [all-seeds seeds]
  (remove (set seeds) all-seeds))

(defn init-polyp [])

(defn add-polyp [cell-w coral x y]
  (let [[col row :as coords] (xy->coords cell-w x y)]
    (-> coral
        (assoc-in [coords] {:color [0 255 255 100]}))))

(defn add-seeds-to-coral [cell-w coral seeds]
  (doall
   (reduce (fn [memo {:keys [x y] :as seed}]
             (add-polyp cell-w memo x y))
           coral
           seeds)))

(defn attach-seeds
  [display-h {:keys [motion-seeds coral-size] :as state}]
  (let [{:keys [num-row-bins cell-w]} coral-size
        attaching (filter (fn [{:keys [x y] :as seed}]
                            (is-attaching? num-row-bins cell-w x y))
                          motion-seeds)]
    (-> state
        (update-in [:motion-seeds] #(remove-seeds % attaching))
        (update-in [:coral] #(add-seeds-to-coral cell-w  % attaching)))))

(defn draw-polyp [{:keys [cell-w cell-half-w
                          hex-w hex-half-w
                          hex-y-offset] :as coral-size}
                  [[col row :as coords]
                   {:keys [color] :as polyp}]]
  (apply q/fill color)
  (hex/draw-hex-cell cell-w cell-half-w
                     hex-w hex-half-w
                     hex-y-offset
                     col (- row 1)))

(defn draw-coral [{:keys [coral-size coral]}]
  (q/push-style)
  (dorun
   (map (partial draw-polyp coral-size)
        coral))
  (q/pop-style))
