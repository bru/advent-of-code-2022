;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]
            [clojure.set :as set]))

(clerk/md (u/day-brief 3))

;; ---
;;
;; ### Part 1 solution

;; Let's start by parsing the input.
;;
;; We know each line represents a rucksack, and each rucksack is divided into
;; two compartments of equal capacity.

(def rucksacks
  (->>(u/puzzle-input 3)
      string/split-lines))

;; let's quickly verify that there are no *unbalanced* rucksacks:
(def unbalanced-rucksacks
  (remove #(even? (count %)) rucksacks))

;; now that we're satisfied that at least the rucksacks are all balanced, let's
;; look into the compartments

(defn- compartmentalise [rucksack]
  (let [compartment-size (/ (count rucksack) 2)]
    (split-at compartment-size rucksack)))

(def rucksack-compartments
  (map compartmentalise rucksacks))

;; since we know that the Elf misplaced **exactly one** item per rucksack, we
;; can use sets to quickly find the common item

(defn- find-duplicates
  "given two strings `a` and `b`, return characters (items) that appear in both."
  [a b]

  (let [a-set (into #{} a)
        b-set (into #{} b)]
    (set/intersection a-set b-set)))

;; remember, just one duplicate per rucksack
(defn find-duplicate [[a b]]
  (first (find-duplicates a b)))

;; let's test it
(find-duplicate (first rucksack-compartments))

;; cool, how about the rest of the rucksacks?

(def all-misplaced-items
  (map find-duplicate rucksack-compartments))

;; and finally, what's the priority of these items? Let's first calculate a
;; single item's priority.
;;
;; lowercase characters in Clojure can be converted to their integer equivalent
;; in the ASCII table, with lowercase characters starting at 97, and uppercase
;; characters starting at 65
(int \a)

(int \A)

;; so we can just infer from the ASCII code if we are working with a lowercase
;; or an uppercase, and infer the right priority value:

(defn item-priority [i]
  (let [item-code (int i)
        lower-case? (< 96 item-code)]
    (if lower-case?
      (- item-code 96)
      (+ 26 (- item-code 64)))))

(item-priority \a)
(item-priority \A)

;; which in total gives

(def total-priority
  (u/style-result
    (reduce + (map item-priority all-misplaced-items))))

;; ---
;;
;; ### Part 2 solution

;; we need to find the badge items for all Elf groups of three elves.
;; This might be a good time to exercise some recursion...

;; ...but first, let's build the facility to identify the badge of a given group

(defn find-badge [[a b c]]
  (let [a-set (set a)
        b-set (set b)
        c-set (set c)]
    (first (set/intersection a-set b-set c-set))))

(find-badge ["zap" "asd" "aaaaargh"])

;; now we can use that recursive loop to consume the pile of rucksacks, 3 at a
;; time, and find the badge in the group.
(def all-badges
  (loop [rucksack-pile rucksacks
         badges []]
    (if (empty? rucksack-pile)
      badges

      (let [[this-group-sacks other-rucksacks] (split-at 3 rucksack-pile)
            this-badge (find-badge this-group-sacks)]
        (recur other-rucksacks (conj badges this-badge))))))

(def badges-priority
  (u/style-result (reduce + (map item-priority all-badges))))

;; having scratched the recursive itch, let's now do the same thing with my
;; usual map reduce approach (when all you have is a hammer... right?)
;;
;; ...but look at how neat is it
(->> rucksacks
     (partition 3)
     (map find-badge)
     (map item-priority)
     (reduce +)
     (u/style-result))

;; next step, transducers? Another time maybe.
