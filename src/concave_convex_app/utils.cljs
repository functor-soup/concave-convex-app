(ns concave-convex-app.utils)

;; takes a seq
;; outputs a seq that is the original seq (missing the last element) zipped 
;; with a once-forward-shifted version of itself (missing the first element)
;; example [1,2,3] becomes [[1,2],[2,3]]
(defn zipz [x]
  (cond
    (empty? x) []
    (= (count x) 1) []
    :else (let [p1 (butlast x)
                p2 (rest x)]
            (map vector p1 p2))))

;; calculates the euclidean distance between
;; two points (x and y are assumed to be maps of 
;; the form {:x some-value-x :y some-value-y})
(defn euclidean-dist [x y]
  (let [xcomp (js/Math.pow (- (:x x) (:x y)) 2)
        ycomp (js/Math.pow (- (:y x) (:y y)) 2)]
    (js/Math.sqrt (+ xcomp ycomp))))

(defn fuzzy-match [x points threshold]
  (filter (fn [z] (< (euclidean-dist x z) threshold)) points))

(defn debug-dists [x y]
   (map #(euclidean-dist x %) y))

(defn make-vector [z1 z2]
   {:x (- (:x z2) (:x z1)) :y (- (:y z2) (:y z1))})

(defn make-vectors [list-of-points]
  (->> (zipz list-of-points) 
       (map #(apply make-vector %))))

;; calculates the cross-product of two
;; two-dimensional vectors
;; inputs are assumed to be maps of the form {:x some-value-x :y some-value-y}
;; formula is k(x1.y2 - y1.x2)
(defn twod-crossprod [x y]
  (let [x1 (:x x)
        y1 (:y x)
        x2 (:x y)
        y2 (:y y)]
    (- (* y2 x1) (* y1 x2))))

(defn same-direction-vectors? [points-list]
  (let [vectors     (make-vectors points-list)
        cross-prods (->> (zipz vectors)
                         (map (fn [[z1 z2]] (twod-crossprod z1 z2)))
                         (map #(js/Math.sign %)))
        fst         (first cross-prods)]
    (every? #(= % fst) cross-prods)))

(defn concave-or-convex? [x]
  (if (same-direction-vectors? x)
     "convex"
     "concave"))

;; checks to see if the polygon drawn by the user
;; closes at the first vertex or some other vertex
;; (return false if it closes at any other vertex other
;; than the vertex vertex)
(defn properly-closed? [x]
  (let [freqs (-> (rest x)
                  (frequencies))] 
    (every? (fn [[k v]] (= 1 v)) freqs)))


