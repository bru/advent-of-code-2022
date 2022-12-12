;; # 🎄 Advent of Clerk: Day 11
(ns advent-of-clerk.day-11
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]))

;; like yesterday, today's [brief](https://adventofcode.com/2022/day/11) is pretty long.


;; ---
;;
;; ## Solution
;;
;; ### Part 1
;;
;; What shape to give to the data?
;;

(def sample-input
  ["Monkey 0:"
   "Starting items: 79, 98"
   "Operation: new = old * 19"
   "Test: divisible by 23"
   " If true: throw to monkey 2"
   " If false: throw to monkey 3"
   ""
   "Monkey 1:"
   "Starting items: 54, 65, 75, 74"
   "Operation: new = old + 6"
   "Test: divisible by 19"
   " If true: throw to monkey 2"
   " If false: throw to monkey 0"
   ""
   "Monkey 2:"
   "Starting items: 79, 60, 97"
   "Operation: new = old * old"
   "Test: divisible by 13"
   " If true: throw to monkey 1"
   " If false: throw to monkey 3"
   ""
   "Monkey 3:"
   "Starting items: 74"
   "Operation: new = old + 3"
   "Test: divisible by 17"
   " If true: throw to monkey 0"
   " If false: throw to monkey 1"])

;; as a general note, the regular expressions in the code below are not optimised.
;; Instead, I'm deciding to keep a little context in the pattern to make them a
;; little more self-explanatory.

(defn- parse-id [id-line]
  (last (re-find #" (\d+):$" id-line)))

(defn- parse-items [items-line]
  (->> items-line
       (re-seq #"\ (\d+),?")
       (map second)
       (map bigint)
       vec))

(defn- parse-op [op-line]
  (let [[_ operation-s operator-s] (re-find #"old\ ([\*\+])\ (\d+|old)" op-line)
        operation (case operation-s
                    "+" +
                    "*" *)
        operator (read-string operator-s)]
    (fn [n] (operation n (if (= operator 'old) n operator)))))

(defn- parse-test [test-line true-branch-line false-branch-line]
  (let [test-operator (read-string (last (re-find #"by\ (\d+)" test-line)))
        match-monkey #(read-string (last (re-find #"monkey\ (\d+)" %)))
        true-monkey (match-monkey true-branch-line)
        false-monkey (match-monkey false-branch-line)]
    [test-operator
     (fn [x]
       (if (zero? (mod x test-operator))
         true-monkey
         false-monkey))]))

(defn parse-monkey-lines [monkey-lines]
  (let [id-line (first monkey-lines)
        id (parse-id id-line)

        items-line (nth monkey-lines 1)
        items (parse-items items-line)

        op-line (nth monkey-lines 2)
        op (parse-op op-line)

        test-line (nth monkey-lines 3)
        true-branch-line (nth monkey-lines 4)
        false-branch-line (nth monkey-lines 5)
        [test-n test] (parse-test test-line true-branch-line false-branch-line)]

    {:id id
     :items items
     :op op
     :test test
     :test-n test-n
     :inspections 0}))

;; NOTE: this version of `parse-input` has been reworked after seeing part 2 of
;; the exercise, to accomodate for the custom relief function.
(defn parse-input
  ([input]
   (parse-input input #(quot % 3)))
  ([input relief-fn]
   (->> (partition 7 7 "" input)
     (map parse-monkey-lines)
     (reduce #(assoc-in %1 [:monkeys (:id %2)] %2) {:relief relief-fn}))))

(parse-input sample-input)

;; now on to doing the rounds.
;;
;; I'll use an atom to keep the monkeys state, and pass that to my
;; "monkey round resolution" functions.


(defn monkey-item [state monkey item]
  (let [id (:id monkey)
        op (:op monkey)
        relief (:relief @state)
        new-val (-> item
                    op
                    relief)
        throw-to (str ((:test monkey) new-val))]
    (swap! state update-in [:monkeys id :inspections] inc)
    (swap! state update-in [:monkeys throw-to :items] conj new-val)))

(defn monkey-round [state monkey-id]
  (let [monkey (get-in @state [:monkeys monkey-id])
        items (:items monkey)]
    (swap! state assoc-in [:monkeys monkey-id :items] [])
    (doseq [i items]
      (monkey-item state monkey i))))

(defn round [state]
  (let [monkeys (sort (keys (:monkeys @state)))]
    (doseq [monkey-id monkeys]
      (monkey-round state monkey-id))))

(defn monkey-business [state]
  (let [monkeys (keys (:monkeys @state))]
    (->> monkeys
      (map #(get-in @state [:monkeys % :inspections]))
      sort
      reverse
      (take 2)
      (reduce * 1))))

(let [state (atom (parse-input sample-input))]
  (dotimes [_n 20]
    (round state))
  (monkey-business state))

;; and now with real input

(def input
  (string/split-lines (u/puzzle-input 11)))

(u/style-result
  (let [state (atom (parse-input input))]
    (dotimes [_n 20]
      (round state))
    (monkey-business state)))

;; ---
;;
;; ### Part 2

;; (let [state (atom (parse-input sample-input (fn [n] n)))]
;;   (dotimes [_n 1000]
;;     (round state))
;;   (monkey-business state))
;;
;; NOPE! The numbers get way too big even for BigInt to handle properly.
;;
;; Let's redefine the relief function in such a way to keep the worry value
;; within constraints... after all in Part 2 we don't care about its exact
;; values...
;;

(defn safeguard [state]
  (let [monkeys (:monkeys @state)
        tests-lcm (->> monkeys
                       vals
                       (map :test-n)
                       (reduce * 1))]
    (fn [n]
      (mod n tests-lcm))))

(let [state (atom (parse-input sample-input))
      relief-fn (safeguard state)]
  (swap! state assoc :relief relief-fn)
  (dotimes [_n 10000]
    (round state))
  (monkey-business state))

;; much better!
;;
;; now with real input:

(u/style-result
  (let [state (atom (parse-input input))
         relief-fn (safeguard state)]
     (swap! state assoc :relief relief-fn)
     (dotimes [_n 10000]
       (round state))
     (monkey-business state)))
