;; # 🎄 Advent of Clerk: Day 10
(ns advent-of-clerk.day-10
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]))

;; today's daily brief is very long, it's best consumed at the [AoC's site](https://adventofcode.com/2022/day/10).

(def sample-input
  (string/split-lines (slurp "inputs/day-10-sample.txt")))

(defn tick [state]
  (update state :readings conj (:x state)))

(defn noop [state]
  (tick state))

(defn addx [state n]
  (-> state
      tick
      tick
      (update :x + n)))

(def initial-state
  {:x 1
   :readings []})

(defn parse-instruction [line]
  (string/split line #" "))

(defn process-input [input state]
  (let [input-line (first input)]
    (if input-line

      (let [instruction (parse-instruction input-line)
            new-state (case (first instruction)
                        "noop" (noop state)
                        "addx" (addx state (read-string (second instruction))))]
        (recur (rest input) new-state))

      state)))


(def final-state
  (process-input sample-input initial-state))

(def sample-cycles [20 60 100 140 180 220])

(defn sample-signal-strength [input]
  (let [state (process-input input initial-state)]
    (for [c sample-cycles]
        (* c (nth (:readings state) (dec c))))))

(reduce + (sample-signal-strength sample-input))

;; cool, that works!


;; now with the real input:
(def input
  (string/split-lines (u/puzzle-input 10)))

(u/style-result
 (reduce + (sample-signal-strength input)))

(defn pixels [state]
  (for [t (range 0 240)]
    (let [pos (mod t 40)
          x (nth (:readings state) t)]
       (if (>= 1 (abs (- pos x)))
         "#" "."))))

(defn paint-screen [input]
  (let [state (process-input input initial-state)
        pxs (pixels state)]
    (->>
     (map string/join (partition 40 pxs))
     (string/join "\n"))))

(paint-screen input)
