;; # 🎄 Advent of Code: Day 4
(ns advent-of-clerk.day-04
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]
            [clojure.set :as set]))

(clerk/md (u/day-brief 4))

;; ---
;;
;; ## Solution
;;
;; ### Part 1
;;
;; As usual, let's load up our input
;;

(def assignments
  (->> (u/puzzle-input 4)
       string/split-lines
       (map (fn [line]
              (let [[_ from1 to1 from2 to2] (re-find #"(\d+)-(\d+),(\d+)-(\d+)" line)]
                [[(read-string from1) (read-string to1)]
                 [(read-string from2)  (read-string to2)]])))))

;; now our job is just to check, for each assignment, whether one is the subset
;; of the other.
;;
;; The temptation to be lazy and solve this just with sets is strong...
(defn- any-subset? [[a b]]
  (or (set/subset? a b)
      (set/subset? b a)))

(def assignments-sets
  (map (fn [[[from-a to-a] [from-b to-b]]]
         [(into #{} (range from-a (inc to-a)))
          (into #{} (range from-b (inc to-b)))])
       assignments))

(->> assignments-sets
     (map-indexed #(when (any-subset? %2) %1))
     (filter identity)
     count
     u/style-result)

;; however, given we're working with ranges, indulging in sets feels a bit
;; excessive...
;;
;; we can solve the "is a contained wihin b or vice-versa?" conundrum with
;; a simple `cond`
(defn fully-enclosed? [[[from-a to-a] [from-b to-b]]]
  (cond
    ; if the two ranges have either the lower or the upper end in common,
    ; one will be fully contained by the other
    (or (= from-a from-b)
        (= to-a to-b)) true

    ; if the lower end of A is greather than that of B, then only A can be
    ; contained in B and is so only if its upper end is less than that of B
    (> from-a from-b) (< to-a to-b)

    ; we now know that A starts lower than B, then B is can be contained by A,
    ; if its upper end is lower than that of A
    :else (> to-a to-b)))

(->> assignments
     (map-indexed #(when (fully-enclosed? %2) %1))
     (filter identity)
     count
     u/style-result)

;; ---
;;
;; ### Part 2
;;

;; now the elves want to know how many assignments overlap, even just partially.
;; We can tweak `fully-enclosed?` above to return just that.

(defn overlap? [[[from-a to-a] [from-b to-b]]]
  (if (<= from-a from-b)
    (>= to-a from-b)
    (>= to-b from-a)))

(->> assignments
     (map-indexed #(when (overlap? %2) %1))
     (filter identity)
     count
     u/style-result)
