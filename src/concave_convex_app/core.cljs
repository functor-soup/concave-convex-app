(ns concave-convex-app.core
    (:require [quil.core :as q :include-macros true]
              [concave-convex-app.utils :as u]
              [dommy.core :as d :refer-macros [sel1]]))

(enable-console-print!)

(println "This text is printed from src/concave-convex-app/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce atom-inital-state {:points [] :allow-user-interaction true :result nil})

(defonce app-state (atom atom-inital-state)) 
(defonce threshold 20)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn draw []
    (q/background 200)
    (q/fill 0)
    (let [points (get @app-state :points)
          zpoints (u/zipz points)]
      (doseq [[z1 z2] zpoints]
       (q/line (:x z1) (:y z1) (:x z2) (:y z2)))
      (doseq [point points]
        (q/ellipse (:x point) (:y point) 20 20))
      (if (not (nil? (:result @app-state)))
       (q/text (:result @app-state) 0 10))))

(defn my-mouse-clicked []
  (if (:allow-user-interaction @app-state)
    (let [x (q/mouse-x)
          y (q/mouse-y)
          point {:x x :y y}
          points (:points @app-state)
          msg (str "Mouse was clicked at " x " and " y)
          fuzz (u/fuzzy-match point points threshold)
          final-point (if (empty? fuzz)
                        point
                        (first fuzz))]
      (do
        (swap! app-state update-in [:points] #(conj % final-point))
        (let [result-message (cond 
                                (and (= final-point (first points)) (> (count points) 1)) 
                                     (u/concave-or-convex? (:points @app-state))
                                (not (u/properly-closed? (:points @app-state)))
                                    "Strange non-closed polygon detected! redo!!"
                                :else nil)]
                (when (not (nil? result-message))
                  (swap! app-state update-in [:allow-user-interaction] not) 
                  (swap! app-state assoc :result result-message)))
        (println msg)))
    (println "Results stage! Click clear to start over")))


(q/defsketch hello
    :draw draw
    :host "chicken"
    :size [500 500]
    :mouse-clicked my-mouse-clicked)

(defn click-handler []
  (reset! app-state atom-inital-state))

(d/listen! (sel1 :#reset) :click click-handler)
