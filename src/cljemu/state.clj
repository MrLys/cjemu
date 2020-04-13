(ns cljemu.state
  (:require [clojure.tools.logging :as log]
            [cljemu.utils :as u])
  (:import  [javafx.scene.paint Color]))


(defn random-set [arr]
  (let [a2 arr
        n (count arr)]
    (doseq [i (range (inc n))]
      (aset a2 i 1))
    a2))

(def key-count 16)

(def register-count 16)

(def memory-size 4096)

(defn get-initial-state [width height]
  {:display (int-array (* width height))
   :width width
   :height height
   :debug true
   :running true
   :terminated false
   :trace []
   :sprites_start 0x050
   :key_count key-count
   :key_input (u/empty-vec key-count)
   :memory (u/empty-vec memory-size)
   :gpio (u/empty-vec register-count)
   :stack (u/empty-vec 512) ; not sure how big the stack needs to be
   :stack-pointer 0
   :sound_timer 0
   :delay_timer 0
   :index 0
   :loaded-rom false
   :draw-flag false
   :rom ""
   :pc 0})

(defn position-to-index [state x y]
  (+ (* y (:width @state)) x))

(defn get-val [state x y]
  (nth (:display @state) 
                (position-to-index state x y)))
(defn get-fill [state x y]
  (if (= 1 (get-val state x y))
    Color/WHITE
    Color/BLACK))

(defn set-fill 
  ([state x y i]
   (let [display (:display @state)]
     (aset display (position-to-index state x y) i)
     (swap! state assoc :display display)))
  ([state x y] (set-fill state x y 1)))


(defn clear-fill [state x y]
  (set-fill state x y 0))

(defn get-display [state]
  (when (:debug @state) (log/info (into [] (:display @state)))))

