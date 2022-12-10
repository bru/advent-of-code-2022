;; # 🎄 Advent of Clerk: Day 8
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]))

(u/day-brief 8)

;; ---
;;
;; ## Solution
;;
;; ### Part 1
;;
;; I have a feeling there is a clever way to solve this, but today I can just
;; come up with the naive approach:
;;
;; - start at position 2,2 and for each item in 2,2 - n-1,m-1:
;; - look in the four directions, if any is free, add 1 to the global counter
;;
;; or, make a poor man's raycaster: build four matrices corresponding to the
;; perspective from N,S,E,W, and then OR all of them. This is probably the
;; simplest solution, but at o(n*m*4) I have a feeling it's not particularly
;; efficient.
;;
;; the only optimisation I can think of is that when moving from tree x to x+1:
;; - if x is visible and is lower than x+1, x+1 will be visible too.
;; - if x is not visible and then x+1 will not be visible (from that side).
;;
;; 🤔 then maybe instead of just keeping a counter, I could build a map of the
;; tree visibility status, and use that to speed up the checks in all directions!
;;
;; Worth exploring... but first, let's do the naive version.
;;
(def sample-input
 ["30373"
  "25512"
  "65332"
  "33549"
  "35390"])

;; first, some helper functions to move around the grid
(defn- to-int [c]
  (if (char? c)
    (- (int c) 48)
    c))

(defn get-xy [the-map [x y]]
  (to-int (nth (nth the-map y) x)))

(defn set-xy [the-map [x y] val]
  (assoc-in the-map [y x] val))

;; Example
(get-xy sample-input [0 0])

;; then to look around

(defn up [[x y]]
  [x (dec y)])

(defn left [[x y]]
  [(dec x) y])

(defn down [[x y]]
  [x (inc y)])

(defn right [[x y]]
  [(inc x) y])

;; with these I should be able to move along pretty easily
(->> [1 1]
  right
  right
  down
  (get-xy sample-input))

;; I'll define a visibility map as a matrix of the same dimensions
;; of the input
(defn new-visibility-map [rows columns]
  (->> (for [x (range 0 columns)
               y (range 0 rows)]
         (or (zero? x) (zero? y) (= (dec columns) x) (= (dec rows) y)))
       (partition columns)
       (map vec)
       vec))

;; and a quick&dirty way to check if we're stepping out of the map
(defn- out-of-map? [amap coords]
  (let [size (count amap)]
    (or (>= (first coords) size)
        (>= (second coords) size)
        (< (first coords) 0)
        (< (second coords) 0))))

;; now, the core functions.
;;
;; First I want a way to cast a ray through a whole row or column, and update the
;; vizmap accordingly
(defn look-from [from direction height-map visibility-map]
  (loop [[x y :as coords] (direction from)
         threshold (get-xy height-map from)
         vizmap visibility-map]
    (if (out-of-map? height-map coords)
      vizmap

      (let [height (get-xy height-map coords)
            vis? (> height threshold)
            new-threshold (if vis? height threshold)
            new-vizmap (update-in vizmap [y x] (some-fn true?) vis?)]
        (recur (direction coords) new-threshold new-vizmap)))))

;; now we can aggregate all our rays in a certain direction, and have a complete
;; perspective from one of the sides.
(defn landscape-toward [direction height-map visibility-map]
  (let [size (count visibility-map)
        starting-point-builder (fn [i]
                                 (condp = direction
                                   right [0 i]
                                   down [i 0]
                                   left [(dec size) i]
                                   up [i (dec size)]))]
    (loop [starting-points (map starting-point-builder (range 0 size))
           vizmap visibility-map]
      (let [[starting-point & other-starting-points] starting-points]
        (if starting-point
          (recur
           other-starting-points
           (look-from starting-point direction height-map vizmap))
          vizmap)))))

;; finally, combining all four sides, we have the complete picture...
(defn survey [height-map]
  (loop [vizmap (new-visibility-map (count height-map) (count (first height-map)))
         [direction & other-directions] [right down left up]]
    (if direction
      (recur (landscape-toward direction height-map vizmap) other-directions)
      vizmap)))

(time
 (->> (survey sample-input)
    flatten
    (filter true?)
    count))

(def input
  (->
   (u/puzzle-input 8)
   string/split-lines))

(u/style-result
  (time (->> (survey input)
          flatten
          (filter true?)
          count)))

;; that, with all its immutable data, takes ~30ms on my laptop.
;;
;; how about with transient data?

(defn- transient-visibility-map [rows columns]
  (->> (for [x (range 0 columns)
               y (range 0 rows)]
         (or (zero? x) (zero? y) (= (dec columns) x) (= (dec rows) y)))
       (partition columns)
       (map (comp transient vec))
       vec
       transient))

(defn- look-from! [from direction height-map vizmap]
  (loop [[x y :as coords] (direction from)
         threshold (get-xy height-map from)]
    (if (out-of-map? height-map coords)
      vizmap

      (let [height (get-xy height-map coords)
            vis? (> height threshold)
            new-threshold (if vis? height threshold)]
        (assoc! (nth vizmap y) x (or (get-xy vizmap [x y]) vis?))
        (recur (direction coords) new-threshold)))))

(let [vizmap (transient-visibility-map 3 3)]
  (assoc! (nth vizmap 1) 1 5)
  (map persistent! (persistent! vizmap)))

(defn- landscape-toward! [direction height-map visibility-map]
  (let [size (count height-map)
        starting-point-builder (fn [i]
                                 (condp = direction
                                   right [0 i]
                                   down [i 0]
                                   left [(dec size) i]
                                   up [i (dec size)]))]
    (loop [starting-points (map starting-point-builder (range 0 size))
           vizmap visibility-map]
      (let [[starting-point & other-starting-points] starting-points]
        (if starting-point
          (recur
           other-starting-points
           (look-from! starting-point direction height-map vizmap))
          vizmap)))))

(defn survey! [height-map]
  (loop [vizmap (transient-visibility-map (count height-map) (count (first height-map)))
         [direction & other-directions] [right down left up]]
    (if direction
      (recur (landscape-toward! direction height-map vizmap) other-directions)
      (->> vizmap
           persistent!
           (map persistent!)))))

(time (->> (survey! input)
         flatten
         (filter true?)
         count))

;; meh, not much difference: the average is ~25ms 🤷
;;
;; ---
;;
;; ### Part 2

;; it's getting late, so let's just throw a variant of `look-from` at the inner
;; square trees, and see what we get.

(defn visibility [from direction height-map]
  (let [height (get-xy height-map from)]
    (loop [coords (direction from)
           distance 0]
        (if (out-of-map? height-map coords)
          distance
          (let [this-height (get-xy height-map coords)
                new-distance (inc distance)]
            (if (>= this-height height)
              new-distance
              (recur (direction coords) new-distance)))))))

(defn lookaround [from height-map]
  (->> [up right down left]
    (map #(visibility from % height-map))
    (reduce * 1)))

(visibility [1 2] left sample-input)
(lookaround [2 3] sample-input)

;; now to do that all over the map...

(defn find-best-view [height-map]
  (let [rows (count height-map)
        cols (count (first height-map))]
    (->> (for [x (range 0 cols)
               y (range 0 rows)]
           (lookaround [x y] height-map))
      sort
      last)))

(find-best-view sample-input)

(u/style-result
  (find-best-view input))
