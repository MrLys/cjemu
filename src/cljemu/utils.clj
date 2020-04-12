(ns cljemu.utils
  (:require [clojure.tools.logging :as log]))

(defn log-i [v]
  (log/info v)
  v)

(defn- func-name [f] 
  (clojure.string/replace (second (re-find #"^.+\$(.+)\@.+$" (str f))) #"\_QMARK\_" "?"))

(defn debug [state args]
  (when (:debug @state)
    (log/info args)))

(defn info [args]
    (log/info args))

(defn error [state f msg]
  (log/error 
    (str "(" f "): " msg " @ " 
         "pc: " (:pc @state)
         "index: " (:index @state)
         "gpio: " (into [] (:gpio @state))
         "memory: " (into [] (:memory @state)))))

(defn pop-stack [state]
  (if (> 1 (count (:stack @state)))
    (error state "pop-stack" "Tried popping empty stack")
  (let [value (peek (:stack @state))
        new-stack (pop (:stack @state))]
        (debug state (str "popping stack value: " value))
        (swap! state assoc :stack new-stack)
        value)))
(defn peek-stack [state]
  (peek (:stack @state)))
(defn read-reg [state index]
  (if (> index (count (:gpio @state))) 
    (error state "read-reg" (str "Reading register out of bound " index))
    (nth (:gpio @state) index)))

(defn update-reg [state index v]
  (let [new-regs 
        (vec (concat 
               (subvec (:gpio @state) 0 index) 
               (vec [v]) 
               (subvec (:gpio @state) (inc index))))]
    (info (str "Setting register " index " to " v))
    (swap! state assoc :gpio new-regs)
    state))

(defn update-memory [state index v]
  (debug state (str "state " (type state) " index " (type index) " v " (type v)))
  (let [new-memory (vec (concat
                            (subvec (:memory @state) 0 index)
                            (vec [v])
                            (subvec (:memory @state) (inc index))))]
    (debug state (format (str "Setting memory at address " index " to %x") v))
    (swap! state assoc :memory new-memory)
    state))
  
(defn read-memory [state i]
  (nth (:memory @state) i))

(defn read-input [state k]
  (when (> k (:key_count @state))
    (error state "read-input" (str "invalid key value " k)))
  (nth (:key_input @state) k))

(defn setf [state v] 
  (info (str "(setf) setting flag register to " v))
  (update-reg state 15 v)
  state)

(defn push-stack [state v]
  (if (<= 16 (count (:stack @state))) 
    (error state "push-stack" (str "Tried pushing " v " onto full stack"))
    (let [new-stack (conj (:stack @state) v)]
      (debug state (str "pushing " v " onto stack!"))
      (swap! state assoc :stack new-stack)
      state)))

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

(defn load-rom [state path]
  (info (str "Loading rom " path))
  (let [rom (slurp-bytes path)]
    (doto state
      (swap! assoc :rom rom)
      (read-rom-to-memory rom 0))))
  
    



