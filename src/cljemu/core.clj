(ns cljemu.core
  (:gen-class)
  (:require [cljfx.api :as fx]
            [cljemu.state :refer :all])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.canvas GraphicsContext]
           [javafx.scene.paint Color]))
(def height 32)
(def width 64)

(def p-size 16)

(def *state (atom (get-initial-state width height)))

(defn draw [^GraphicsContext graphics state x y]
  (doto graphics 
      (.setFill (get-fill state x y))
      (.fillRect (* x p-size) (* y p-size) p-size p-size)))

(defn draw-canvas [^Canvas canvas state]
  (let [graphics (.getGraphicsContext2D canvas)]
  (loop [i (- (:height state) 1)]
    (when (>= i 0)
      (loop [j (- (:width state) 1)]
        (when (>= j 0)
          (draw graphics state j i)
          (recur (- j 1))))
    (recur (- i 1))))))

(defn fill [state]
  (let [s (doto state
    (assoc :display (set-fill state 0 0 ))
    (assoc :display (set-fill state 0 31 ))
    )]
  (let [rs (reset! *state s)]
      (get-display s)
      (get-display *state)
      )))

(defn handle [event]
  (let [st @*state 
        {:keys [event/type]} event]
    (case type
      ::set-fill (fill st))))

(def actual-handler
  (-> handle
      (fx/wrap-co-effects {:state #(deref *state)})))

(defn canvas-screen [{:keys [state width height]}]
  {:fx/type :canvas
   :width width
   :height height
   :draw (fn [canvas]
           (draw-canvas canvas state))})


(def renderer
  (fx/create-renderer
    :middleware
    (fx/wrap-map-desc
      (fn [canvas-state]
        {:fx/type :stage
         :showing true
         :title "Emulator"
         :width (+ (* p-size width) 120)
         :height (+ (* p-size height) 120)
         :scene {:fx/type :scene
                 :root {:fx/type :v-box
                        :padding 50
                        :spacing 10
                        :children [{:fx/type canvas-screen
                                    :width (* p-size width)
                                    :height (* p-size height)
                                    :value canvas-state
                                    :state canvas-state
                                    :on-value-changed #(reset! *state %)}
                                   {:fx/type :button
                                    :text "update-canvas"
                                    :on-action (fn [^Canvas canvas] (actual-handler {:event/type ::set-fill}))}]}}}))))

(defn -main
  [& args]
  (fx/mount-renderer *state renderer))
