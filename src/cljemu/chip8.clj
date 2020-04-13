(ns cljemu.chip8
  (:require [cljemu.utils :as u]
            [cljemu.state :as s]))

(def sprites
  [0xF0 0x90 0x90 0x90 0xF0; 0
   0x20 0x60 0x20 0x20 0x70; 1
   0xF0 0x10 0xF0 0x80 0xF0; 2
   0xF0 0x10 0xF0 0x10 0xF0; 3
   0x90 0x90 0xF0 0x10 0x10; 4
   0xF0 0x80 0xF0 0x10 0xF0; 5
   0xF0 0x80 0xF0 0x90 0xF0; 6
   0xF0 0x10 0x20 0x40 0x40; 7
   0xF0 0x90 0xF0 0x90 0xF0; 8
   0xF0 0x90 0xF0 0x10 0xF0; 9
   0xF0 0x90 0xF0 0x90 0x90; A
   0xE0 0x90 0xE0 0x90 0xE0; B
   0xF0 0x80 0x80 0x80 0xF0; C
   0xE0 0x90 0x90 0x90 0xE0; D
   0xF0 0x80 0xF0 0x80 0xF0; E
   0xF0 0x80 0xF0 0x80 0x80]); F
(defn VX [opcode]
  (bit-and 0x000F (bit-shift-right (bit-and opcode 0x0F00) 8)))

(defn VY [opcode]
  (bit-and 0x000F (bit-shift-right (bit-and opcode 0x00F0) 4)))

(defn cls [state opcode] ; OK
  "Clear the display."
  (u/debug (format "cls %x" opcode))
  (swap! state assoc :display (int-array (* (:width @state) (:height @state))))
  (u/inc-pc state))

(defn ret [state opcode]
    "00EE - RET"
    "Return from a subroutine."
    "The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer."
    (u/debug (format "ret %x" opcode))
      (swap! state assoc :pc (u/peek-stack state))
      (u/pop-stack state)
      (swap! state assoc :draw-flag true))

(defn jmp [state opcode]
  "1nnn - JP addr
  Jump to location nnn.

  The interpreter sets the program counter to nnn."
  (u/debug (format "jmp %x" opcode))
  (let [nnn (bit-and opcode 0x0fff)]
  (swap! state assoc :pc nnn)))

(defn call-addr [state opcode]
  "2nnn - CALL addr
  Call subroutine at nnn.

  The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to nnn."
  (u/debug (format "call-addr %x" opcode))
  (let [nnn (bit-and opcode 0x0FFF)]
    (u/inc-pc state)
    (u/push-stack state (:pc @state))
    (swap! state assoc :pc nnn)))


(defn se [state opcode]
  "3xkk - SE Vx, byte
  Skip next instruction if Vx = kk.
  The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2."
  (u/debug (format "se %x" opcode))
  (let [Vx (VX opcode)
        kk (bit-and opcode 0x00ff)]
    (when (= (u/read-reg state Vx) kk) ; skip if equal
      (u/inc-pc state))
      (u/inc-pc state)))


(defn sne [state opcode]
  "4xkk - SNE Vx, byte
  Skip next instruction if Vx != kk.

  The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2."
  (u/debug (format "sne %x" opcode))
  (let [Vx (VX opcode)
        kk (bit-and opcode 0x00ff)]
    (when (not (= (u/read-reg state Vx) kk))
      (u/inc-pc state))
      (u/inc-pc state)))


(defn se-reg [state opcode]
  "5xy0 - SE Vx, Vy
  Skip next instruction if Vx = Vy.

  The interpreter compares register Vx to register Vy, and if they are equal, increments the program counter by 2."
  (u/debug (format "se-reg %x" opcode))
  (let [Vx (VX opcode)
        Vy (VY opcode)]
    (when (= (u/read-reg state Vx) (u/read-reg state Vy))
      (u/inc-pc state))
    (u/inc-pc state)))

(defn ld [state opcode]
  "6xkk - LD Vx, byte
  Set Vx = kk.

  The interpreter puts the value kk into register Vx."
  (u/debug (format "ld %x" opcode))
  (let [Vx (VX opcode)
        kk (bit-and opcode 0x00ff)]
    (u/update-reg state Vx kk)
    (u/inc-pc state)))

(defn add [state opcode]
  "7xkk - ADD Vx, byte
  Set Vx = Vx + kk.

  Adds the value kk to the value of register Vx, then stores the result in Vx."
  (u/debug (format "add %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        kk (bit-and opcode 0x00ff)
        result (+ current-vx kk)]
    (u/debug (format "result: %d" result))
    (u/update-reg state Vx (bit-and result 0xff))
    (u/inc-pc state)))

(defn ld-reg [state opcode]
  "8xy0 - LD Vx, Vy
  Set Vx = Vy.

  Stores the value of register Vy in register Vx."
  (u/debug (format "ld-reg %x" opcode))
  (let [Vx (VX opcode)
        Vy (VY opcode)]
    (u/update-reg state Vx (u/read-reg state Vy))
    (u/inc-pc state)))

(defn or-reg [state opcode]
  "8xy1 - OR Vx, Vy
  Set Vx = Vx OR Vy.

  Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx. 
  A bitwise OR compares the corrseponding bits from two values, and if either bit is 1, then the same bit in the result is also 1. Otherwise, it is 0."
  (u/debug (format "or-reg %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        Vy (VY opcode)
        current-vy (u/read-reg state Vy)]
    (u/update-reg state Vx (bit-or current-vx current-vy))
    (u/inc-pc state)))

(defn and-reg [state opcode]
  "8xy2 - AND Vx, Vy
  Set Vx = Vx AND Vy.

  Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx. 
  A bitwise AND compares the corrseponding bits from two values, and if both bits are 1, then the same bit in the result is also 1. Otherwise, it is 0."
  (u/debug (format "and-reg %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        Vy (VY opcode)
        current-vy (u/read-reg state Vy)]
    (u/update-reg state Vx (bit-and current-vx current-vy))
    (u/inc-pc state)))

(defn xor-reg [state opcode]
  "8xy3 - XOR Vx, Vy
  Set Vx = Vx XOR Vy.

  Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx. 
  An exclusive OR compares the corrseponding bits from two values, and if the bits are not both the same, 
  then the corresponding bit in the result is set to 1. Otherwise, it is 0."
  (u/debug (format "xor-reg %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        Vy (VY opcode)
        current-vy (u/read-reg state Vy)]
    (u/update-reg state Vx (bit-xor current-vx current-vy))
    (u/inc-pc state)))


(defn add-reg [state opcode]
  "8xy4 - ADD Vx, Vy
  Set Vx = Vx + Vy, set VF = carry.

  The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. 
  Only the lowest 8 bits of the result are kept, and stored in Vx."
  (u/debug (format "add-reg %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        Vy (VY opcode)
        current-vy (u/read-reg state Vy)
        sum (+ current-vx current-vy)]
      (if (> sum 255) (u/setf state 1) (u/setf state 0))
      (u/update-reg state Vx (bit-and sum 0xFF))
    (u/inc-pc state)))

(defn sub-reg [state opcode]
  "8xy5 - SUB Vx, Vy
  Set Vx = Vx - Vy, set VF = NOT borrow.

  If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx."
  (u/debug (format "sub-reg %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        Vy (VY opcode)
        current-vy (u/read-reg state Vy)
        sum (- current-vx current-vy)]
    (if (> current-vx current-vy) (u/setf state 1) (u/setf state 0))
        (u/update-reg state Vx (bit-and sum 0xFF))
        (u/inc-pc state)))

(defn shr [state opcode]
  "8xy6 - SHR Vx {, Vy}
  Set Vx = Vx SHR 1.

  If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2."
  (u/debug (format "shr %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        quotient (bit-shift-right current-vx 1)]
    (u/setf state (bit-and 0x1 current-vx))
    (u/update-reg state Vx quotient)
    (u/inc-pc state)))

(defn subn [state opcode]
  "8xy7 - SUBN Vx, Vy
  Set Vx = Vy - Vx, set VF = NOT borrow.

  If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx."
  (u/debug (format "subn %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        Vy (VY opcode)
        current-vy (u/read-reg state Vy)
        diff (- current-vy current-vx)]
    (u/update-reg state Vx diff)
    (if (> current-vy current-vx) (u/setf state 1) (u/setf state 0))
        (u/update-reg state Vx (bit-and 0xFFFF diff))
    (u/inc-pc state)))

(defn shl [state opcode]
  "8xyE - SHL Vx {, Vy}
  Set Vx = Vx SHL 1.

  If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2."
  (u/debug (format "shl %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        result (bit-shift-left current-vx 1)]
    (u/update-reg state Vx (bit-and 0xFFFF result))
    (u/setf state (bit-and (bit-shift-right current-vx 7) 0x1))
    (u/inc-pc state)))

(defn sne-reg [state opcode]
  "9xy0 - SNE Vx, Vy
  Skip next instruction if Vx != Vy.

  The values of Vx and Vy are compared, and if they are not resequal, the program counter is increased by 2."
  (u/debug (format "sne-reg %x" opcode))
  (let [Vx (VX opcode)
        Vy (VY opcode)]
    (when (not (= (u/read-reg state Vx)
                  (u/read-reg state Vy)))
      (u/inc-pc state))
      (u/inc-pc state)))

(defn ld-i [state opcode]
  "Annn - LD I, addr
  Set I = nnn.

  The value of register I is set to nnn."
  (u/debug (format "ld-i %x" opcode))
  (let [nnn (bit-and opcode 0x0fff)]
    (swap! state assoc :index nnn)
      (u/inc-pc state)))

(defn jp-v0 [state opcode]
  "Bnnn - JP V0, addr
  Jump to location nnn + V0.

  The program counter is set to nnn plus the value of V0."
  (u/debug (format "jp-v0 %x" opcode))
  (let [nnn (bit-and opcode 0x0fff)
        V0 (u/read-reg state 0)]
    (swap! state assoc :pc (+ nnn V0))))

(defn rnd [state opcode]
  "Cxkk - RND Vx, byte
  Set Vx = random byte AND kk.

  The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. 
  The results are stored in Vx. See instruction 8xy2 for more information on AND."
  (u/debug (format "rnd %x" opcode))
  (let [Vx (VX opcode)
        kk (bit-and opcode 0x00ff)]
    (u/update-reg state Vx (bit-and (rand-int 256) kk))
    (u/inc-pc state)))

(defn drw [state opcode]
  "Dxyn - DRW Vx, Vy, nibble
  Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.

  The interpreter reads n bytes from memory, starting at theresation on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites."
  (u/debug (format "drw %x" opcode))
  (let [Vx (VX opcode)
        Vy (VY opcode)
        x (u/read-reg state Vx)
        y (u/read-reg state Vy)
        n (bit-and opcode 0x000f)
        I (:index @state)
        display (:display @state)]
    (u/setf state 0)
    (loop [i 0]
      (when (< i n)
        (let [Byte (u/read-memory state (+ I i))]
          (loop [j 0]
            (when (< j 8)
              ;(u/info (format "x:%d y:%d i:%d j:%d Y:%d X:%d" x y i j (+ y i) (+ x j)))
              (let [Y (mod (+ y i) 32)
                    X (mod (+ x j) 64)]
                (s/set-fill state 
                            X
                            Y
                            (bit-xor 
                              (nth display (s/position-to-index state X Y)) 
                              (bit-and 0x0001 (bit-shift-right Byte (- 8 j)))))
                  (u/setf state (s/get-val state X Y)))
                (recur (inc j)))))
        (recur (inc i))))
    ;(u/info "Finito in drw")
    (u/inc-pc state)))

(defn skp [state opcode]
  "Ex9E - SKP Vx
  Skip next instruction if key with the value of Vx is pressed.

  Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2."
  (u/debug (format "skp %x" opcode))
  (let [Vx (VX opcode)]
    (when (= (u/read-input state (u/read-reg state Vx)) 1) 
      (u/inc-pc state))
    (u/inc-pc state)))

(defn sknp [state opcode]
  "ExA1 - SKNP Vx
  Skip next instruction if key with the value of Vx is not pressed.

  Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2."
  (u/debug (format "sknp %x" opcode))
  (let [Vx (VX opcode)]
    (when (= (u/read-input state (u/read-reg state Vx)) 0) 
      (u/inc-pc state))
    (u/inc-pc state)))

(defn ld-dt [state opcode]
  "Fx07 - LD Vx, DT
  Set Vx = delay timer value.

  The value of DT is placed into Vx."
  (u/debug (format "ld-dt %x" opcode))
  (let [Vx (VX opcode)]
    (u/update-reg state Vx (:delay_timer @state))
    (u/inc-pc state)))

(defn ld-k [state opcode]
  "Fx0A - LD Vx, K
  Wait for a key press, store the value of the key in Vx.

  All execution stops until a key is pressed, then the value of that key is stored in Vx."
  (let [Vx (VX opcode)]
        (loop [frst (first (filter #(= (nth (:key_input @state) %1) 1) (doseq [i (range 0 (count (:key_input @state)))])))]
          (when (not (nil? frst))
            (u/update-reg state Vx frst)
            (recur (first (filter #(= (nth (:key_input @state) %1) 1) (doseq [i (range 0 (count (:key_input @state)))]))))))
    (u/inc-pc state)))

(defn ld-dt-reg [state opcode]
  "Fx15 - LD DT, Vx
  Set delay timer = Vx.

  DT is set equal to the value of Vx."
  (u/debug (format "ld-dt-reg %x" opcode))
  (let [Vx (VX opcode)]
    (swap! state assoc :delay_timer (u/read-reg state Vx))
    (u/inc-pc state)))

(defn ld-st-reg [state opcode]
  "Fx18 - LD ST, Vx
  Set sound timer = Vx.

  ST is set equal to the value of Vx."
  (u/debug (format "ld-st-reg %x" opcode))
  (let [Vx (VX opcode)]
    (swap! state assoc :sound_timer (u/read-reg state Vx))
    (u/inc-pc state)))

(defn add-i [state opcode]
  "Fx1E - ADD I, Vx
  Set I = I + Vx.

  The values of I and Vx are added, and the results are stored in I."
  (u/debug (format "add-i %x" opcode))
  (let [Vx (VX opcode)
        I (:index @state)
        current-vx (u/read-reg state Vx)]
    (if (> (+ I current-vx) 0xFFFF) (u/setf state 1) (u/setf state 0))
    (swap! state assoc :index (+ I current-vx))
    (u/inc-pc state)))

(defn ld-sprite [state opcode]
  "Fx29 - LD F, Vx
  Set I = location of sprite for digit Vx.

  The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx. See section 2.4, Display, for more information on the Chip-8 hexadecimal font."
  (u/debug (format "ld-sprite %x" opcode))
  (let [Vx (VX opcode)
        current-vx (u/read-reg state Vx)
        I (* current-vx 5)]
    (swap! state assoc :index I)
    (u/inc-pc state)))

(defn ld-b [state opcode]
  "Fx33 - LD B, Vx
  Store BCD representation of Vx in memory locations I, I+1, and I+2.

  The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2."
  (u/debug (format "ld-b %x" opcode))
  (let [Vx (VX opcode)
        I (:index @state)
        current-vx (u/read-reg state Vx)
        hundreds (int (/ (mod current-vx 1000) 100))
        tens (int (/ (mod current-vx 100) 10))
        ones (int (mod current-vx 10))]
   (u/update-memory state I hundreds) 
   (u/update-memory state (inc I) tens) 
   (u/update-memory state (+ I 2) ones)
   (u/inc-pc state)))

(defn ld-to-memory [state opcode]
  "Fx55 - LD [I], Vx
  Store registers V0 through Vx in memory starting at location I.

  The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I."
  (u/debug (format "ld-to-memory %x" opcode))
  (let [Vx (VX opcode)
        I (:index @state)]
   (loop [i 0]
      (when (<= i Vx)
        (u/update-memory state (+ i I) (u/read-reg state i))
       (recur (inc i))))
   ;(swap! state assoc :index (+ (:index @state) Vx 1))
   (u/inc-pc state)))

(defn ld-from-memory [state opcode]
  "Fx65 - LD Vx, [I]
  Read registers V0 through Vx from memory starting at location I.

  The interpreter reads values from memory starting at location I into registers V0 through Vx."
  (u/debug (format "ld-from-memory %x" opcode))
  (let [Vx (VX opcode)
        I (:index @state)]
    (loop [x 0]
      (when (<= x Vx)
        (u/update-reg state x (u/read-memory state (+ x I)))
        (recur (inc x))))
    ;(swap! state assoc :index (+ (:index @state) Vx 1))
    (u/inc-pc state)))

(def instructions 
  {0x00E0 cls
   0x00EE ret    
   0x1000 jmp
   0x2000 call-addr
   0x3000 se
   0x4000 sne
   0x5000 se-reg
   0x6000 ld
   0x7000 add
   0x8000 ld-reg
   0x8001 or-reg
   0x8002 and-reg
   0x8003 xor-reg
   0x8004 add-reg
   0x8005 sub-reg
   0x8006 shr
   0x8007 subn
   0x800E shl
   0x9000 sne-reg
   0xA000 ld-i
   0xB000 jp-v0
   0xC000 rnd
   0xD000 drw
   0xE00E skp
   0xE0A1 sknp
   0xF007 ld-dt
   0xF00A ld-k
   0xF015 ld-dt-reg
   0xF018 ld-st-reg
   0xF01E add-i
   0xF029 ld-sprite
   0xF033 ld-b
   0xF055 ld-to-memory
   0xF065 ld-from-memory
   })
(defn handle-MSByte-E [opcode state]
  (let [LSByte (bit-and 0x000F opcode)]
    (case LSByte 
      0x000E (get instructions 0xE00E)
      0x0001 (get instructions 0xE0A1)
      (do (u/error state "handle-MSByte-E" (format "Invalid opcode %x" opcode))  (throw (Exception. (format "Invalid opcode (special) %x" opcode)))))))
(defn handle-MSByte-F [opcode state]
  (let [LSByte (bit-and 0x000F opcode)]
    (case LSByte 
      0x0007 (get instructions 0xF007)
      0x000A (get instructions 0xF00A)
      0x0008 (get instructions 0xF018)
      0x000E (get instructions 0xF01E)
      0x0009 (get instructions 0xF029)
      0x0003 (get instructions 0xF033)
      0x0005 (let [Byte (bit-and 0x00FF opcode)]
               (case Byte 
                 0x0055 (get instructions 0xF055)
                 0x0065 (get instructions 0xF065)
                 0x0015 (get instructions 0xF015)
                 (do (u/error state "handle-MSByte-F" (format "Invalid opcode %x" opcode))  (throw (Exception. (format "Invalid opcode (special) %x" opcode))))))
      (do (u/error state "handle-MSByte-F" (format "Invalid opcode %x" opcode))  (throw (Exception. (format "Invalid opcode (special) %x" opcode)))))))


(defn handle-MSByte-8 [opcode state]
  (let [LSByte (bit-and 0x000F opcode)]
    (case LSByte 
      0x0000 (get instructions 0x8000)
      0x0001 (get instructions 0x8001)
      0x0002 (get instructions 0x8002)
      0x0003 (get instructions 0x8003)
      0x0004 (get instructions 0x8004)
      0x0005 (get instructions 0x8005)
      0x0006 (get instructions 0x8006)
      0x0007 (get instructions 0x8007)
      0x000E (get instructions 0x800E)
      (do (u/error state "handle-MSByte-8" (format "Invalid opcode %x" opcode)) (throw (Exception. (format "Invalid opcode (special) %x" opcode)))))))

(defn to-instruction [opcode state]
  ; special case for 0xF0ij 
  (let [MSByte (bit-and (bit-shift-right opcode 12) 0x000F)]
    (case MSByte
      0x0001 (get instructions 0x1000)
      0x0002 (get instructions 0x2000)
      0x0003 (get instructions 0x3000) 
      0x0004 (get instructions 0x4000)
      0x0005 (get instructions 0x5000)
      0x0006 (get instructions 0x6000)
      0x0007 (get instructions 0x7000)
      0x0009 (get instructions 0x9000)
      0x000A (get instructions 0xA000)
      0x000B (get instructions 0xB000)
      0x000C (get instructions 0xC000)
      0x000D (get instructions 0xD000)
      0x0008 (handle-MSByte-8 opcode state)
      0x000E (handle-MSByte-E opcode state)
      0x000F (handle-MSByte-F opcode state)
   (do (u/error state "to-instruction" (format "Invalid opcode %x" opcode)) (throw (Exception. (format "Invalid opcode (special) %x" opcode)))))))

    
(defn perform-opcode [state nibble-one nibble-two]
    (let [opcode (bit-or
                   (bit-and 0xff00 (bit-shift-left nibble-one 8))
                   (bit-and nibble-two 0x00ff))
          instruction (case opcode
                        (0x00E0 0x0EE) (get instructions opcode)
                        (to-instruction opcode state ))]
      (swap! state assoc :trace (conj (:trace @state) (format "%x" opcode)))
      (instruction state opcode)))
  
(defn cycle-once [state]
  (let [nibble-one (nth (:memory @state) (:pc @state))
        nibble-two (nth (:memory @state) (+ (:pc @state) 1))
        new-delay (if (> (:delay_timer @state) 0) (dec (:delay_timer @state)) 0)
        new-sound (if (> (:sound_timer @state) 0) (dec (:sound_timer @state)) 0)]
    (perform-opcode state nibble-one nibble-two)
    (swap! state assoc :delay_timer new-delay)
    (swap! state assoc :sound_timer new-sound)))
