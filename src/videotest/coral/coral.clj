(ns videotest.coral.coral)


;; Should have a chance to attach, when:
;; - at bottom,
;; - run into existing coral,
;; - 

(defn is-bottom? [display height y]
  )

(defn is-occupied? [x y]
  )

(defn bumping-coral? []
  )

(defn is-attaching? [x y]
  (> 0.5 (rand)))

(defn remove-seeds [all-seeds seeds]
  (remove (set seeds) all-seeds))

(defn attach-seeds
  [display-h {:keys [motion-seeds coral] :as state}]
  (let [attaching (filter (fn [{:keys [x y] :as seed}]
                            (is-attaching? x y))
                          motion-seeds)]
    (-> state
        (update-in [:motion-seeds] #(remove-seeds % attaching))))
  )
