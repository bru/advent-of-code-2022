;; # 🎄 Advent of Code: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]))

(clerk/md (u/day-brief 1))

;; ---
;;
;; ## Solution
;;
;; ### Part 1

;; first of all, let's keep our "Elves manifest" (the puzzle input) handy.
(def manifest
  (-> (slurp (u/input-path 1))
   string/split-lines))

;; I want a way to parse the manifest into a list of data structures
;; representing each elf and their carried goods.
;;
;; For each Elf, I want a record of:
;;
;; - their position in the manifest
;; - the value of each 🍬 carried
;; - the total calories carried
;;
(defn assign-bag [idx bag]
  {:pos idx
   :items bag
   :tot (reduce + bag)})

;; next we can parse the manifest, grouping all items carried by the same
;; Elf, and then calling `assign-bag` for each group to generate our desired
;; data representation of the Elf.
;;
;; Grouping can be seen as a reducing function, where we start with a vector
;; containing an empty vector (the first group), then we look at the each
;; item and we either add it to last group (the "current" one), or append a new
;; empty vector, depending on whether the new item is a number or an empty line
(defn- group-by-elf [elves item]
    (if (empty? item)
      (conj elves [])
      (update elves (dec (count elves)) conj (read-string item))))

;; Parsing the manifest is then just a case of applying `group-by-elf` to the
;; manifest, and then mapping `assign-bag` to the resulting collection of elves:
;;
(defn parse-manifest [manifest]
  (->> manifest
    (reduce group-by-elf [[]])
    (map-indexed assign-bag)))

(def elves (parse-manifest manifest))

;; let's see how that looks like in a table:
(clerk/table elves)

;; We've been asked to find the Elf carrying the most calories, so let's sort
;; that list accordingly:
(def sorted-elves
  (reverse (sort-by :tot elves)))

;; ...and as a table:
(clerk/table sorted-elves)

;; and the winner will be at the top of the list.
;;
;; The actual Calories are to be found under its `:tot` key.
(def top-calories-elf
    (u/style-result (:tot (first sorted-elves))))

;; ### Part 2
;;
;; in part 2 we want to know the top three Elves carrying the most Calories.
;;
;; Having already calculated the list of Elves sorted by Calories, makes this
;; trivial: we'll just have to take the first 3 elements of `sorted-elves`,
;; extract the `:tot` values, and sum them up.
(def top-three-total
  (let [total (->> sorted-elves
                (take 3)
                (map :tot)
                (reduce +))]
    (u/style-result total)))
