;; # 🎄 Advent of Clerk: Day 9
(ns advent-of-clerk.day-09
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]))

(u/day-brief 9)

;; ---
;;
;; ## Solution
;;
;; ### Part 1
;;
;; Let's build on yesterday's small library of functions to move in different
;; directions:

(defn up [[x y]]
  [x (dec y)])

(defn left [[x y]]
  [(dec x) y])

(defn down [[x y]]
  [x (inc y)])

(defn right [[x y]]
  [(inc x) y])

;; We also need a way for the tail to keep up with the head

(defn- close? [a b]
  (and
   (>= 1 (abs (- (first a) (first b))))
   (>= 1 (abs (- (second a) (second b))))))

(defn- move-toward [from to]
  (let [dx (abs (- (first from) (first to)))
        dy (abs (- (second from) (second to)))
        x (cond
            (= 2 dx) (/ (+ (first from) (first to)) 2)
            (= 2 dy) (first to)
            :else (first from))
        y (cond
            (= 2 dy) (/ (+ (second from) (second to)) 2)
            (= 2 dx) (second to)
            :else (second from))]
    [x y]))

(defn trail [head tail-knots]
  (loop [starts tail-knots
         ends [head]]
    (let [from (first starts)
          to (last ends)]
      (if from
        (let [new-from
              (if (close? to from)
                from
                (move-toward from to))]
          (recur (rest starts) (conj ends new-from)))
        ends))))

;; let's test this with the sample input:
;;
(def sample-input
  ["R 4"
   "U 4"
   "L 3"
   "D 1"
   "R 4"
   "D 1"
   "L 5"
   "R 2"])

(defn- move [{:keys [knots breadcrumbs] :as state} direction moves]
  (if (> moves 0)
    (let [new-head (direction (first knots))
          new-knots (trail new-head (rest knots))
          new-breadcrumbs (conj breadcrumbs (last new-knots))]
      (recur {:knots new-knots :breadcrumbs new-breadcrumbs}
             direction (dec moves)))
    state))

(defn tail-breadcrumbs
  ([input]
   (tail-breadcrumbs input 1))
  ([input tail-knots]
   (loop [[line & other-lines] input
          state {:knots (vec (repeat (inc tail-knots) [0 0])) :breadcrumbs [[0 0]]}]
     (if line
       (let [[dir num] (string/split line #" ")
             direction (case dir
                         "R" right
                         "U" up
                         "L" left
                         "D" down)
             state (move state direction (read-string num))]
         (recur other-lines state))

       (:breadcrumbs state)))))


(-> (tail-breadcrumbs sample-input)
  distinct
  count)

(def input
  (->
   (u/puzzle-input 9)
   string/split-lines))

(-> (tail-breadcrumbs input)
    distinct
    count)

;; ---
;;
;; ### Part 2
;;
;;

(-> (tail-breadcrumbs sample-input 9)
    distinct
    count)

(def sample-input-2
  ["R 5"
   "U 8"
   "L 8"
   "D 3"
   "R 17"
   "D 10"
   "L 25"
   "U 20"])

(-> (tail-breadcrumbs sample-input-2 9)
    distinct
    count)

(-> (tail-breadcrumbs input 9)
    distinct
    count)
