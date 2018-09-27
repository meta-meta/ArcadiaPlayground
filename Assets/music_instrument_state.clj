(ns music-instrument-state
  (:require [osc :as o])
  (:use [arcadia.core]
        [clojure.set :only [difference union]]))

; Make sure "Filter Duplicates" is unchecked in OscIn component

(def blank-128-map (->> (range 128)                         ;TODO: use sparse data instead of vel 0?
                        (map #(vector % 0))
                        (flatten)
                        (apply hash-map)))

(def s (let [inst-initial {
                           :cc        blank-128-map
                           :notes     blank-128-map
                           :listeners #{}
                           }]
         (atom {
                :default    inst-initial
                :a-300      inst-initial
                :keystation inst-initial
                })))

(defn- on-midi-evt [instrument event osc-msg]
  (let [[index val] (vec (. osc-msg (get_args)))
        listeners (get-in @s [instrument :listeners])]
    (swap! s assoc-in [instrument event index] val)
    (doseq [listener listeners] (listener event index val))
    (log (str "/" instrument "/" event " " index " " val))))

(o/listen "/a-300/note" (fn [osc-msg] (on-midi-evt :a-300 :note osc-msg)))
(o/listen "/keystation/note" (fn [osc-msg] (on-midi-evt :keystation :note osc-msg)))

(defn listen
  "registers a listener for instrument events. listener must accept args: midi-evt index val"
  [instrument listener]
  (log (str instrument listener))
  (swap! s update-in [instrument :listeners] #(union % #{listener}))
  )

(defn get-notes
  "returns map of currently played notes and their velocities"
  ([instrument]
   (->> (get-in @s [instrument :notes])
        (filter #(> (second %) 0))
        (flatten)
        (apply hash-map)))
  ([] (get-notes :default)))

(defn get-note "returns current velocity of note"
  ([instrument n] (get-in @s [instrument :notes n]))
  ([n] (get-note :default)))