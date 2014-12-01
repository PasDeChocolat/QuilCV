(ns videotest.basic-mover.bin-lattice
  (:require
   [videotest.basic-mover.native-vector :as fvec]))

(defn bin-things-near [bin-size bin-tree loc]
  (let [[x y] (fvec/x-y loc)
        col (int (/ x bin-size))
        row (int (/ y bin-size))
        bins (for [i (range -1 2)
                   j (range -1 2)
                   :let [bin-col (+ col i)
                         bin-row (+ row j)]]
               [bin-col bin-row])]
    (vec
     (doall
      (reduce (fn [memo bin]
                (concat memo (get bin-tree bin [])))
              [] bins)))))

(defn loc->bin [location bin-size]
  (let [[x y] (fvec/x-y location)]
    [(int (/ x bin-size))
     (int (/ y bin-size))]))
