(ns videotest.falling.cv-draw
  (:import
   [org.opencv.core Core MatOfPoint Scalar]
   [java.util ArrayList]))


(defn draw-poly-with-pts
  "Draw a filling polygon with the given points and color."
  [img-mat color glyph-pts]
  (let [poly (MatOfPoint.)
        c (apply (fn [r g b a]
                   (Scalar. r g b a))
                 color)]
    (.fromList poly (ArrayList. glyph-pts))
    (Core/fillPoly img-mat (ArrayList. [poly]) c)))

