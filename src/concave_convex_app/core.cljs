(ns concave-convex-app.core
    (:require [quil.core :as q :include-macros true]))

(enable-console-print!)

(println "This text is printed from src/concave-convex-app/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:points [{:x 166 :y 166} {:x 200 :y 300}]}))

;; ================== utils ======================================

(defn zipz [x]
  (cond
    (empty? x) []
    (= (count x) 1) []
    :else (let [p1 (butlast x)
                p2 (rest x)]
            (map vector p1 p2))))

;; ===============================================================

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn draw []
    (q/background 200)
    (q/fill 0)
    (let [points (get @app-state :points)
          zpoints (zipz points)]
      (doseq [[z1 z2] zpoints]
       (q/line (:x z1) (:y z1) (:x z2) (:y z2)))
      (doseq [point points]
        (q/ellipse (:x point) (:y point) 20 20))))

(defn my-mouse-clicked []
  (let [x (q/mouse-x)
        y (q/mouse-y)
        msg (str "Mouse was clicked at " x " and " y)]
    (do
       (swap! app-state update-in [:points] #(conj % {:x x :y y}))
       (println msg) 
      )))


(q/defsketch hello
    :draw draw
    :host "chicken"
    :size [500 500]
    :mouse-clicked my-mouse-clicked)
