(ns music-instrument-state
  (:use [arcadia.core]
        [osc :only [listen]]))

; Make sure "Filter Duplicates" is unchecked in OscIn component

(def s (atom {
              :notes (->> (range 128) ;TODO: use sparse data instead of vel 0?
                          (map #(vector % 0))
                          (flatten)
                          (apply hash-map))
              }))

(defn- on-note [osc-msg]
  (let [[note vel] (vec (. osc-msg (get_args)))]
    (swap! s assoc-in [:notes note] vel)
    (log (str note " " vel))))

(listen "/note" #'on-note)

(defn get-notes
  "returns map of currently played notes and their velocities"
  []
  (->> (:notes @s)
       (filter #(> (second %) 0))
       (flatten)
       (apply hash-map)))

(defn get-note "returns current velocity of note"
  [n] (get (:notes @s) n))
