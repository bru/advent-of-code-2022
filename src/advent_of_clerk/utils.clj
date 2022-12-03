(ns advent-of-clerk.utils
  (:require [nextjournal.clerk :as clerk]))

(defn input-path [day]
  (format "inputs/day-%02d.txt" day))

(defn puzzle-input [day]
  (let [path (input-path day)]
    (slurp path)))

(defn brief-path [day]
  (format "briefs/day-%02d.md" day))

(defn day-brief [day]
  (let [path (brief-path day)]
    (slurp path)))

(defn style-result [result]
  (let [decorated-result (str "✨✨✨ " result " ✨✨✨")]
    (clerk/html
       [:button.bg-red-500.hover:bg-red-700.text-white.rounded-xl.px-2.py-1 decorated-result])))
