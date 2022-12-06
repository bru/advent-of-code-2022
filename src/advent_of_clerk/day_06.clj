;; # 🎄 Advent of Code: Day 6
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]
            [advent-of-clerk.utils :as u]))

(clerk/md (u/day-brief 6))

;; ---
;;
;; ## Solution
;;
;; ### Part 1
;;
;; ok, today's interesting. The naive solution would be to go through the input
;; one char at a time, keep a FIFO buffer of four chars (or rolling pointers to
;; the input string), and check the exit condition (no duplicates in the buffer)
;; at each round.
;;
;; That works, and with a buffer length of 4 it may be acceptable, but I bet we
;; can do better.
;;
;; How about trading some space for speed? If I create an index map of the input
;; characters, where I record their "last seen at" position, I should be able to
;; complete the algorithm in linear time.
;;

;; First though, let's get hold of the input.

(def input
  (->
   (u/puzzle-input 6)
   string/split-lines
   first))

;; we know the marker length, we'll use this a lot.
(def marker-length 4)

;; now, about finding that marker.
;;
;; I can achieve that with a loop, having for parameters:
;;
;; - the characters index map
;; - a cursor providing the position of the next character to be examined
;; - a counter of the "marker candidate" characters so far.
;;
;; the exit conditions will be:
;;
;; 1. we found the marker 🎉
;; 2. end of input 😢
;;
;; otherwise, it's just another loop in the life of the algorithm, and its
;; work is to check if the current character has been previously seen within
;; the previous `marker-length` characters. If so, it invalidates the current
;; marker assumption, and it proceeds resetting the counter to the difference
;; between the cursor and the position where the character was previously seen
;; at.
;; Otherwise - i.e. if the current character didn't appear recently - it
;; increments counter and recurs.
;;

(defn find-start-after
  [input marker-length]
  (let [input-length (count input)]
    (loop [index {}
           cursor 0
           counter 0]
      (cond
        (= marker-length counter) cursor
        (= cursor input-length) nil
        :else
        (let [current (get input cursor)
              previously-seen-at (get index current)
              valid? (or
                      (nil? previously-seen-at)
                      (< previously-seen-at (- cursor counter)))
              new-counter (if valid?
                            (inc counter)
                            (- cursor previously-seen-at))]
          (recur (assoc index current cursor) (inc cursor) new-counter))))))

;; And here it comes
(u/style-result
  (find-start-after input marker-length))


;; ---
;;
;; ### Part 2
;;
;; It should be just a matter of changing the length of the pattern the function
;; looks for...
;;
(def message-length 14)

(u/style-result
  (find-start-after input message-length))
