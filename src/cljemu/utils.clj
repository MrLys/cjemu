(ns cljemu.utils
  (:require [clojure.tools.logging :as log]))

(defn log-i [v]
  (log/info v)
  v)

(defn- func-name [f] 
  (clojure.string/replace (second (re-find #"^.+\$(.+)\@.+$" (str f))) #"\_QMARK\_" "?"))

(defn debug [state args]
  (when (:debug state)
    (log/debug args)))
(defn error [state f msg]
  (log/error 
    (str "(" f "): " msg " @ " 
         "pc: " (:pc state)
         "index: " (:index state)
         "gpio: " (into [] (:gpio state))
         "memory: " (into [] (:memory state)))))

(defn pop-stack [state]
  (if (> 1 (count (:stack state)))
    (error state "pop-stack" "Tried popping empty stack")
  (let [value (peek (:stack state))
        new-stack (pop (:stack state))]
        (debug state (str "popping stack value: " value))
        (assoc state :stack new-stack))))

(defn update-reg [gpio index v]
  (vec (concat (subvec gpio 0 index) (vec [v]) (subvec gpio (inc index)))))

(defn setf [state v] 
  (let [new-gpio (update-reg (:gpio state) 15 v)]
    (debug state (str "setting flag register to " v))
    (assoc state :gpio new-gpio)))
(defn push-stack [state v]
  (if (<= 16 (count (:stack state))) 
    (error state "push-stack" (str "Tried pushing " v " onto full stack"))
    (let [new-stack (conj (:stack state) v)]
      (debug state (str "pushing " v " onto stack!"))
      (assoc state :stack new-stack))))

(defn empty-vec [l]
  (let [v (transient [])]
  (doseq [i (range 0 l)]
     (conj! v 0))
  (persistent! v))) 
