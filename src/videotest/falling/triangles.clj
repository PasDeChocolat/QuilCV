(ns videotest.falling.triangles
  (:import
   [org.opencv.core Point]))


(defn triangle-points
  "Saves origin points for each triangle, which is a vector pair of
   x and y positions in the image material [col row]."
  [num-cols num-rows bin-size]
  (into {}
   (for [row-bin (range 0 num-rows)
         col-bin (range 0 num-cols)
         :let [x (* col-bin bin-size)
               y (* row-bin bin-size)]
         :when (= 0 (mod row-bin 2))]
     [[col-bin row-bin] [x y]])))

(defn triangle-orientations
  "Radian orientations associated to triangle origin points."
  [triangle-points]
  (let [left-cols (filter (fn [[col row]]
                            (even? col))
                          (keys triangle-points))]
   (reduce (fn [memo [col row :as coords]]
             (let [rot1 (rand-int 2)]
               (-> memo
                   (assoc-in [coords] rot1))))
           {}
           left-cols)))

(defn apex-top-right [side-w x y]
  (let [w (- side-w 1)]
    [(Point.    x       y)
     (Point. (+ x w)    y)
     (Point. (+ x w) (+ y w))]))

(defn apex-bottom-left [side-w x y]
  (let [w (- side-w 1)]
    [(Point. (+ x w) (+ y w))
     (Point.    x    (+ y w))
     (Point.    x       y)]))

(defn apex-top-left [side-w x y]
  (let [w (- side-w 1)]
    [(Point.    x    (+ y w))
     (Point.    x       y)
     (Point. (+ x w)    y)]))

(defn apex-bottom-right [side-w x y]
  (let [w (- side-w 1)]
    [(Point. (+ x w)    y)
     (Point. (+ x w) (+ y w))
     (Point.    x    (+ y w))]))

(defn glyph [side-w n x y]
  (cond
   (= n 0)
   (apex-top-left     side-w x y)

   (= n 1)
   (apex-bottom-left  side-w x y)

   (= n 2)
   (apex-bottom-right side-w x y)

   (= n 3)
   (apex-top-right    side-w x y)))

(defn triangle-glyphs
  [side-w triangle-points triangle-orientations]
  (reduce (fn [memo [col row :as coords]]
            (let [rot (if (even? col)
                        (triangle-orientations coords)
                        (mod (+ (triangle-orientations [(dec col) row])
                                2)
                             4))
                  [x y] (if (even? col)
                          (triangle-points coords)
                          (triangle-points [(dec col) row]))]
              (assoc-in memo [coords] (glyph side-w rot x y))))
          {}
          (keys triangle-points)))
