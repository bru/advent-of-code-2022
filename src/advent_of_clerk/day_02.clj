;; # 🎄 Advent of Code: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]))

(clerk/md (u/day-brief 2))

;; ---
;;
;; ## Solution
;;
;; ### Part 1
;;
;; first let's have our strategy guide handy...
;;
(def strategy-guide
  (-> (u/puzzle-input 2)
    string/split-lines))

;; and define the rules of our game. I could just keep the symbols given in the
;; input ("A", "B", "C" for player 1, and "X", "Y", "Z" for player 2), but I am
;; lazy and prefer not to impose extra exercise on my brain, so let's decode
;; everything instead.
;;
(def match-rules
  {:rock      {:rock     :draw
               :paper    :win
               :scissors :lose}
   :paper     {:rock     :lose
               :paper    :draw
               :scissors :win}
   :scissors  {:rock     :win
               :paper    :lose
               :scissors :draw}})

;; what `match-rules` does is to define a rule tree that I can follow with a
;; given input to find the outcome.
;;
;; For example, if player 1 chooses `:paper` and player 2 `:scissors`, the
;; outcome can be easily found with
;;
(get-in match-rules [:paper :scissors])

;; this is useful, let's make it into a function
(defn resolve-match
  "Returns 1 if p1 wins, 2 if p2 wins, 0 if draw."
  [p1 p2]
  (get-in match-rules [p1 p2]))

;; next we want to have a way to decode the choices of the players
;;
(def p1-code
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def p2-code
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

;; and also to extract scores from the move chosen and from the match outcome
;;
(def p2-move-scores
  {:rock     1
   :paper    2
   :scissors 3})

(def match-scores
  {:draw 3
   :lose 0
   :win  6})

;; we're now ready to use all of the above to calculate the score for a certain
;; match given its entry in the strategy guide.
;;
;; the function `match-score` defined below will first split the entry into
;; the move code chosen by the two players, then decode those into the actual
;; move, and use those to resolve the match and calculate the final score.
;;
(defn match-score
  "Given an entry from the strategy guide, returns the score for p2"
  [strategy-entry]
  (let [[p1-input p2-input] (string/split strategy-entry #" ")
        p1-move (get p1-code p1-input)
        p2-move (get p2-code p2-input)
        move-score (get p2-move-scores p2-move)
        outcome-score (get match-scores (resolve-match p1-move p2-move))]
    (+ move-score outcome-score)))

;; in order to get the total score for all matches, we can just sum all the
;; results or, in other words map the strategy guide to the `match-score`
;; function and reduce the results via the `+` function.
(def total-score
  (u/style-result (reduce + (map match-score strategy-guide))))

;; ### Part 2

;; ok, those cheeky elves actually meant something different in the strategy
;; guide.
;;
;; Fine, let's define a new correspondence between the second column code and
;; the desired outcome.
(def desired-outcome-codes
  {"X" :lose
   "Y" :draw
   "Z" :win})

;; now we can create a new rule tree that will give us the move that player 2
;; needs to pick based on the desired outcome and the leaked player 1's move.
(def p2-move-by-desired-outcome
  {:lose {:rock :scissors
          :paper :rock
          :scissors :paper}

   :draw {:rock :rock
          :paper :paper
          :scissors :scissors}

   :win {:rock :paper
         :paper :scissors
         :scissors :rock}})

(defn guided-p2-move [p1-move outcome]
  (get-in p2-move-by-desired-outcome [outcome p1-move]))

;; for example, if the strategy guide asks us to `:lose` and player 1 picks
;; `:rock` ...
(guided-p2-move :rock :lose)

;; the logic to calculate the final match score is similar to `match-score`
;; but it takes into account the desired outcome and the call to
;; `guided-p2-move`.
;;
(defn guided-match-score [strategy-entry]
  (let [[p1-input desired-code] (string/split strategy-entry #" ")
        desired-outcome (get desired-outcome-codes desired-code)
        p1-move (get p1-code p1-input)
        p2-move (guided-p2-move p1-move desired-outcome)
        move-score (get p2-move-scores p2-move)
        outcome-score (get match-scores (resolve-match p1-move p2-move))]
    (+ move-score outcome-score)))

;; again, we can just sum the individual scores to get the final result
(def guided-total-score
  (u/style-result (reduce + (map guided-match-score strategy-guide))))
