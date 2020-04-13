(ns cljemu.utils
  (:require [clojure.tools.logging :as log]))

(defn log-i [v]
  (log/info v)
  v)

(defn- func-name [f] 
  (clojure.string/replace (second (re-find #"^.+\$(.+)\@.+$" (str f))) #"\_QMARK\_" "?"))

(defn debug [args]
  (when false
    (log/info args)))

(defn info [args]
    (log/info args))

(defn debugging [state f msg]
  (log/info (str "(" f "): " msg " @ " 
         " pc: " (:pc @state)
         " index: " (:index @state)
         " gpio: " (into [] (:gpio @state))
         " trace: " (into [] (:trace @state))
         " memory: " (into [] (:memory @state)))))

(defn error [state f msg]
  (log/error 
    (str "(" f "): " msg " @ " 
         " pc: " (:pc @state)
         " index: " (:index @state)
         " gpio: " (into [] (:gpio @state))
         " trace: " (into [] (:trace @state))
         " memory: " (into [] (:memory @state)))))

(defn peek-stack [state]
  (peek (:stack @state)))

(defn pop-stack [state]
  (swap! state assoc :stack (pop (:stack @state))))

(defn push-stack [state v]
  (swap! state assoc :stack (conj (:stack @state) v)))

(defn read-reg [state index]
  (if (> index (count (:gpio @state))) 
    (error state "read-reg" (str "Reading register out of bound " index))
    (bit-and 0xFFFF (nth (:gpio @state) index))))

(defn update-vec-index [vect index v]
  (vec 
    (concat
      (subvec vect 0 index)
      (vec [(bit-and 0xff v)])
      (subvec vect (inc index)))))

(defn update-reg [state index v]
  (swap! state assoc :gpio (update-vec-index (:gpio @state) index v)))

(defn update-memory [state index v]
  (swap! state assoc :memory (update-vec-index (:memory @state) index v))
  state)
  
(defn read-memory [state i]
  (nth (:memory @state) i))

(defn read-input [state k]
  (when (> k (:key_count @state))
    (error state "read-input" (str "invalid key value " k)))
  (nth (:key_input @state) k))


(defn setf [state v] 
  (update-reg state 15 (bit-and 0x1 v)))


(defn empty-vec [l]
  (let [v (transient [])]
  (doseq [i (range 0 l)]
     (conj! v 0))
  (persistent! v))) 

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn read-rom-to-memory [state rom i]
  (if (< i (count rom))
    (read-rom-to-memory (update-memory state (+ 0x200 i) (nth rom i)) rom (inc i))
    state))
(defn increment-pc [state v]
  (swap! state assoc :pc (+ (:pc @state) v)))
(defn skip-ins [state] 
  (increment-pc state 4))
(defn inc-pc [state]
  (increment-pc state 2))

(defn load-rom [state path]
  (let [rom (slurp-bytes path)]
    (doto state
      (swap! assoc :rom rom)
      (read-rom-to-memory rom 0))))
  
    



