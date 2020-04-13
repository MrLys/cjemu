(ns cljemu.core
  (:gen-class)
  (:require [cljfx.api :as fx]
            [cljemu.state :refer :all]
            [cljemu.chip8 :as chip]
            [cljemu.utils :as u])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.canvas GraphicsContext]
           [javafx.scene.paint Color]))
(def height 32)
(def width 64)

(def p-size 4)

(def *state (atom (get-initial-state width height)))

(defn draw [^GraphicsContext graphics state x y]
  (doto graphics 
      (.setFill (get-fill *state x y))
      (.fillRect (* x p-size) (* y p-size) p-size p-size)))

(defn draw-canvas [^Canvas canvas state]
  (let [graphics (.getGraphicsContext2D canvas)]
  (loop [i (- (:height @*state) 1)]
    (when (>= i 0)
      (loop [j (- (:width @*state) 1)]
        (when (>= j 0)
          (draw graphics state j i)
          (recur (- j 1))))
    (recur (- i 1))))))

(defn fill [state]
  (doto state
    (swap! assoc :display (set-fill state 0 0 1))
    (swap! assoc :display (set-fill state 0 31 1))))

(defn handle [event]
  (let [st @*state 
        {:keys [event/type]} event]
    (case type
      ::set-fill (fill *state)
      ::clear-fill (chip/cls *state 0x0000))))

(def actual-handler
  (-> handle
      (fx/wrap-co-effects {:state *state})))

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
         :showing (not (:terminated @*state))
         :title "Emulator"
         :width (+ (* p-size width) 240)
         :height (+ (* p-size height) 240)
         :scene {:fx/type :scene
                 :root {:fx/type :h-box
                        :padding 50
                        :spacing 10
                        :children [{:fx/type :v-box
                                    :children [{:fx/type canvas-screen
                                                :width (* p-size width)
                                                :height (* p-size height)
                                                :value canvas-state
                                                :state canvas-state
                                                :on-value-changed #(reset! *state %)}
                                               {:fx/type :h-box
                                                :children [{:fx/type :button
                                                            :text "pause/unpause"
                                                            :on-action (fn [_] 
                                                                         (swap! *state assoc :running (not (:running @*state))))}
                                                           {:fx/type :button
                                                            :text "Next"
                                                            :on-action (fn [_] (chip/cycle-once *state))}
                                                           {:fx/type :button
                                                            :text "Terminate"
                                                            :on-action (fn [_] (swap! *state assoc :terminated true))}
                                                           {:fx/type :button
                                                            :text "clear"
                                                            :on-action (fn [_] (chip/cls *state 0x0000))}
                                                           {:fx/type :button
                                                            :text "Log"
                                                            :on-action (fn [_] (u/debugging *state "logging" (str "stopping: "(:running @*state))))}]}]}]}}}))))

(defn -main
  [& args]
  (fx/mount-renderer *state renderer)
  (loop [i 0
         j 0]
    (when (< j (count chip/sprites))
      (u/update-memory *state i (nth chip/sprites j))
      (recur (inc i) (inc j))))
  (u/info "Done loading sprites")
  ;(u/load-rom *state "./roms/test_opcode.ch8")
  ;(u/load-rom *state "./roms/zero_demo.ch8")
  ;(u/load-rom *state "./roms/particle_demo.ch8")
  ;(u/load-rom *state "./roms/tetris.ch8")
  ;(u/load-rom *state "./roms/clock_program.ch8")
  (u/load-rom *state "./roms/maze_demo.ch8")
  (u/load-rom *state "./roms/Airplane.ch8")
  ;(u/load-rom *state "./roms/stars.ch8")
  ;(u/load-rom *state "./roms/hires.ch8")
  ;(u/load-rom *state "./roms/BC_test.ch8")
  (swap! *state assoc :pc 0x200)
  (while (not (:terminated @*state)) 
    (when (:running @*state)
      (chip/cycle-once *state))
      (Thread/sleep 7))
  (u/info "Terminating! Goodbye!"))

