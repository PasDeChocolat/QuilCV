(ns videotest.falling.triangles
  (:import
   [org.opencv.core Point]))


(defn triangle-points
  "Saves origin points for each triangle, which is a vector pair of
   x and y positions in the image material [col row]."
  [num-cols num-rows display-bin-size]
  (for [col-bin (range 0 num-cols)
        row-bin (range 0 num-rows)
        :let [mat-col (* col-bin display-bin-size)
              mat-row (* row-bin display-bin-size)]
        :when (= 0 (mod col-bin 2))]
    [mat-col mat-row]))

(defn triangle-orientations
  "Radian orientations associated to triangle origin points."
  [triangle-points]
  (reduce (fn [memo [pt1 pt2]]
            (let [rot1 (rand-int 4)]
              (-> memo
                  (assoc-in [pt1] rot1))))
          {}
          (partition 2 triangle-points)))

(defn apex-top-right [display-bin-size-x2 x y]
  [(Point. x y)
   (Point. (+ x display-bin-size-x2)    y)
   (Point. (+ x display-bin-size-x2) (+ y display-bin-size-x2))])

(defn apex-bottom-left [display-bin-size-x2 x y]
  [(Point. (+ x display-bin-size-x2) (+ y display-bin-size-x2))
   (Point.    x                      (+ y display-bin-size-x2))
   (Point.    x                         y)])

(defn apex-top-left [display-bin-size-x2 x y]
  [(Point.    x                      (+ y display-bin-size-x2))
   (Point.    x                         y)
   (Point. (+ x display-bin-size-x2)    y)])

(defn apex-bottom-right [display-bin-size-x2 x y]
  [(Point. (+ x display-bin-size-x2)    y)
   (Point. (+ x display-bin-size-x2) (+ y display-bin-size-x2))
   (Point.    x                      (+ y display-bin-size-x2))])

(defn glyph [display-bin-size-x2 n x y]
  (cond
   (= n 0)
   (apex-top-right    display-bin-size-x2 x y)

   (= n 1)
   (apex-bottom-right display-bin-size-x2 x y)

   (= n 2)
   (apex-bottom-left  display-bin-size-x2 x y)

   (= n 3)
   (apex-top-left     display-bin-size-x2 x y)))

(defn triangle-glyphs
  [display-bin-size-x2 triangle-points triangle-orientations]
    (reduce (fn [memo [[x y :as pt1] pt2]]
              (let [rot1 (triangle-orientations pt1)
                    rot2 (mod (+ rot1 2) 4)]
              (-> memo
                  (assoc-in [pt1] (glyph display-bin-size-x2
                                         rot1 x y))
                  (assoc-in [pt2] (glyph display-bin-size-x2
                                         rot2 x y)))))
            {}
            (partition 2 triangle-points)))
