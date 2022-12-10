;; # 🎄 Advent of Clerk: Day 7
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as u]
            ;; [clojure.zip :as zip]
            [clojure.string :as string]))

(clerk/md (u/day-brief 7))

;; ---
;;
;; ## Solution
;;
;; ### Part 1
;;
;; Since today I only managed to sit down and check AoC quite late in the
;; night (technically it's the next day already), let's jump out of the hammock
;; and for once embrace the "perfect is the enemy of good" motto. I want to go
;; to sleep 😅.
;;
;; At first I thought today's exercise would present a nice opportunity to
;; practice some [clojure.zip](https://clojuredocs.org/clojure.zip)? 🤔
;;
;; However, since all we want to do is just to extract some branch sizes from the
;; tree, all that state and flexibility is probably superfluous.
;;
;; [Zippers](https://en.wikipedia.org/wiki/Zipper_(data_structure)) are
;; nevertheless awesome and you should check them.
;;
;; Instead, how about we just define our state as a map containing:
;; - the current path as a simple vector of directory names
;; - a hash having the "encoded" dir path as key and its cumulative size as value
;;
;; For example in the sample input, after the first `cd a` line we'll have:
;;
;; ```clojure
;; {:path ["/" "a"]
;;  :directories
;;  {"/" 23352670
;;   "/a" 0
;;   "/d" 0}}}
;; ```
;; When the input is all processed, we should be able to simply filter the
;; `directories` for our condition (size < 100,000)
;;
;; First let's see how the different input lines transform our state
(defn cd-root [state]
  (assoc state :path ["/"]))

(defn ls [state] state)

(defn cd-up [state]
  (update state :path pop))

(defn cd [state name]
  (update state :path conj name))

(defn +file [state name size]
  (loop [curr-state state
         path (:path state)]
    (if (seq path)
      (let [encoded-path (string/join "/" path)
            new-state (update-in curr-state [:directories encoded-path] + size)]
        (recur new-state (pop path)))
      curr-state)))

(defn +dir [state name]
  (let [encoded-path (string/join "/" (conj (:path state) name))
        curr-size (get state encoded-path 0)]
    (update state :directories assoc encoded-path curr-size)))

(defn make-state []
  {:path ["/"] :directories {"/" 0}})

(defn apply-input [state input]
  (loop [curr-state state
         steps input]
    (let [[curr-step & future-steps] steps]
      (if curr-step
        (let [new-state (condp re-find curr-step
                          #"\$ cd \/" (cd-root curr-state)
                          #"\$ ls" (ls curr-state)
                          #"\$ cd \.\." (cd-up curr-state)
                          #"\$ cd (.+)" :>> (fn [[_ name]]
                                              (cd curr-state name))
                          #"(\d+) (.+)" :>> (fn [[_ size name]]
                                              (let [size-n (read-string size)]
                                                (+file curr-state name size-n)))
                          #"dir (.+)"   :>> (fn [[_  name]]
                                              (+dir curr-state name)))]
          (recur new-state future-steps))

        curr-state))))

(def sample-input
 (string/split-lines "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"))

(def sample-state
  (apply-input (make-state) sample-input))


;; finally, to found the "small" folders
(defn find-dirs-smaller-than [state limit]
  (filter #(<= (second %) limit) (:directories state)))

(defn sum-of-small-dirs [state limit]
  (reduce #(+ %1 (second %2))
          0
          (find-dirs-smaller-than state limit)))

;; so our sample result is
(sum-of-small-dirs sample-state 100000)

;; now with real input
(def input
  (->
   (u/puzzle-input 7)
   string/split-lines))

(def puzzle-state
  (apply-input (make-state) input))

(u/style-result
  (sum-of-small-dirs puzzle-state 100000))

;;
;; ---
;;
;; ### Part 2
;;
;;
;; we know the disk size
(def disk-size 70000000)

;; and the update size
(def update-size 30000000)

;; now, given the free space
(def free-space
  (- disk-size (get-in puzzle-state [:directories "/"])))

;; so we can infer how much space we need to free up still
(def required-space
  (- update-size free-space))

;; we can use a similar filter function as the one we used before to isolate
;; all candidates
(defn find-dirs-greather-than [state size]
  (filter #(>= (second %) size) (:directories state)))

;; and we're done, since we don't need to keep the identity of the dir we can
;; just focus on the vals, and pick the smallest!
(u/style-result
  (->> required-space
      (find-dirs-greather-than puzzle-state)
      vals
      sort
      first))
