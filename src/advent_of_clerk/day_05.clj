;; # 🎄 Advent of Code: Day 5
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            [clojure.string :as string]))

(clerk/md (u/day-brief 5))

;; ---
;;
;; ## Solution
;;
;; ### Part 1

;; today the input is split into two parts:
;;
;; 1. the current disposition of the crates
;; 2. the list of moves
;;
;; so, first thing is to find a way to parse that in order to get a working
;; representation of both the map and the moves
;;
;; #### Parse Puzzle Input

(def raw-input
  (-> (u/puzzle-input 5)
      string/split-lines))

;; we can use the first empty line as a divider for the two input sections
;; or we could use regular expressions to identify the line type.
;;
;; I'll go with the former as it's slightly more interesting, and create a
;; sorting function that will update the state, appending the current line
;; either to the crates map or to the moves list, based on the fact that we
;; did or did not already encounter an empty line.

(defn split-sections [input]
  (let [starting-state {:crates-map []
                        :moves []
                        :active-section :crates-map}
        sorting-fn (fn [acc value]
                     (if (empty? value)
                       (assoc acc :active-section :moves)
                       (update acc (:active-section acc) conj value)))]
    (dissoc (reduce sorting-fn starting-state input) :active-section)))

(def sections-input
  (split-sections raw-input))

;; `sections-input` gives us a map with two keys: `:crates-map` and `:moves`.
;; The value of each key is the input for the corresponding section.
;;
;; ...and onto parsing the two sections themselves!
;;
;; ##### Moves
;;
;; I'll start with the `moves` section as it's trivial:
;; a simple regular expression will extract the three values:
;;
;; - quantity
;; - source stack
;; - destination stack

(def move-pattern
  (re-pattern "move (?<quantity>\\d+) from (?<source>\\d+) to (?<destination>\\d+)"))

;; and we can then use the value in the named groups to build friendly maps
;; with the values for each line / move.
;;
;; this is a good exercise, I keep forgetting that in order to populate the groups,
;; I need to call `re-find` on the matcher!
;;
;; (defn parse-move-line [line]
;;   (let [matcher (re-matcher move-pattern line)
;;         _ (re-find matcher)]
;;      {:quantity    (read-string (.group matcher "quantity"))
;;       :source      (read-string (.group matcher "source"))
;;       :destination (read-string (.group matcher "destination"))}))
(defn parse-move-line [line]
  (let [matcher (re-matcher move-pattern line)
        _ (re-find matcher)]
    (zipmap [:quantity :source :destination] (map read-string (rest (re-groups matcher))))))

;; a map is all is required to process the input section
(defn parse-moves-input [moves-input]
    (map parse-move-line moves-input))

(def moves
  (parse-moves-input (:moves sections-input)))

;; which we can nicely lay out on a table, thanks to clerk:
(clerk/table moves)

;; ##### Crates Map
;;
;; now, in the case of the crates map, I want to shape the data like this:
;;
;; ```clojure
;; {1 ["A" "B" "C" "D"]
;;  2 ["W" "X" "Y" "Z"]
;;  ; and so on
;; }
;; ```
;;
;; or in other words a hash map where each key is the name of a stack, and its
;; value is a vector representation of the stack itself.
;;
;; I'll take a shortcut here, and assume the number of stacks is always going to
;; be 9, each labeled with a number from 1 to 9 (as it is in my input).
;;
;; That allows me to skip the step of identifying the number of columns, which
;; is trivial btw.
;;
;; We can use the following formula to find the text column that will hold
;; the crate type given the stack label:
;;
(clerk/tex "column = (label - 1) * 4 + 1")

;; or, in clojure

(defn stack-col [label]
  (+ 1 (* 4 (dec label))))

;; for example
(stack-col 1)
(stack-col 2)

;; given an input line, we'll iterate through the columns and extract the
;; corresponding crate value (if present) from the line, eventually updating
;; each stack in the crates map (note, this is a reducing operation).
;;
;; Note that each line represents a level of the stacks, and not all stacks may
;; have a crate at a given level / line.

(def labels
  (range 1 10))

(defn crate-finder [line]
  (fn [crate-map label]
      (let [column (stack-col label)
            crate (nth line column \space)]
        (if (= crate \space)
          crate-map
          (update crate-map label conj crate)))))

(defn parse-map-line [crate-map line]
  (reduce (crate-finder line) crate-map labels))

;; we can use `parse-map-line` as a reducing function for the input lines
;; collection, which will gives us a full crate map as a result.
;;
(defn parse-map-input [crates-map-input]
  (let [[_header & stack-lines] (reverse crates-map-input)]
    (reduce parse-map-line {} stack-lines)))

(def crates-map
  (parse-map-input (:crates-map sections-input)))

(clerk/table crates-map)

;; #### Moving the crates
;;
;; finally, let's apply the moves to the crate stacks.
;;
;; the core API of our "super crane" is a function called `move-crates` which
;; accepts input in the format we extracted before: a map with keys
;; `:quantity`, `:source`, `:destination`
;;
;; to infer the right coordinates to move the arm, the crane also requires
;; the current state of the crane stacks, which we pass as the first parameter
;; of the `move-crate` function.

(defn move-crate [crates {:keys [quantity source destination]}]
  (let [[arm-crates new-source] (split-at quantity (get crates source))]

    (-> crates
        (assoc source new-source)
        (update destination into arm-crates))))

(def final-crates
  (reduce move-crate crates-map moves))

(clerk/table final-crates)

(def top-crates
  (map #(first (get final-crates %)) labels))

;; ---
;;
;; ### Part 2
;;
;; CrateMove 9001 uh? ok, fine...
;;
;; That actually just requires `reverse`ing the `arm-crates` list passed to the
;; `update` function in `move-crate`. We could also seize the opportunity to
;; refactor this a little bit, but my lunch break is almost over, so I'll just live
;; with the duplication for now.

(defn move-crate-9001 [crates {:keys [quantity source destination]}]
  (let [[arm-crates new-source] (split-at quantity (get crates source))]

    (-> crates
        (assoc source new-source)
        (update destination into (reverse arm-crates)))))

(def final-crates-9001
  (reduce move-crate-9001 crates-map moves))

(clerk/table final-crates-9001)

(def top-crates-9001
  (map #(first (get final-crates-9001 %)) labels))
