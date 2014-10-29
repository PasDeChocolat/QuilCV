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

(defn is-occupied? [coral coords]
  (contains? coral coords))

(defn is-row-under-occupied? [coral [col row]]
  (let [row-under (inc row)]
   (or
    (and (even? col)
         (some #(is-occupied? coral %) [[(dec col) row]
                                        [     col  row-under]
                                        [(inc col) row]]))
    (and (odd? col)
         (some #(is-occupied? coral %) [[(dec col) row-under]
                                        [     col  row-under]
                                        [(inc col) row-under]])))))

(def BOTTOM-ATTACH-PCT 0.1)

(defn is-attaching? [num-rows cell-w coral x y]
  (let [[col row :as coords] (xy->coords cell-w x y)]
    (and (not (is-occupied? coral coords))
         (not (is-occupied? coral [col (dec row)]))
         (or (and (is-bottom? num-rows row)
                  (> BOTTOM-ATTACH-PCT (rand)))
             (is-row-under-occupied? coral coords))
    )))

(defn remove-seeds [all-seeds seeds]
  (remove (set seeds) all-seeds))

(defn init-polyp [color]
  {:color color})

(defn add-polyp [cell-w coral x y color]
  (let [[col row :as coords] (xy->coords cell-w x y)]
    (-> coral
        (assoc-in [coords] (init-polyp color)))))

(defn add-seeds-to-coral [cell-w coral seeds]
  (doall
   (reduce (fn [memo {:keys [x y color] :as seed}]
             (add-polyp cell-w memo x y color))
           coral
           seeds)))

(defn attach-seeds
  [display-h {:keys [motion-seeds coral-size coral] :as state}]
  (let [{:keys [num-row-bins cell-w]} coral-size
        attaching (filter (fn [{:keys [x y] :as seed}]
                            (is-attaching? num-row-bins cell-w coral x y))
                          motion-seeds)]
    (-> state
        (update-in [:motion-seeds] #(remove-seeds % attaching))
        (update-in [:coral] #(add-seeds-to-coral cell-w % attaching)))))

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
