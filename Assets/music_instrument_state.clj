(ns music-instrument-state
  (:use [arcadia.core]
        [osc :only [listen]]))

; Make sure "Filter Duplicates" is unchecked in OscIn component

(def s (let [notes-initial
             {
              :notes (->> (range 128)                       ;TODO: use sparse data instead of vel 0?
                          (map #(vector % 0))
                          (flatten)
                          (apply hash-map))
              }]
         (atom {
                :default notes-initial
                :keystation notes-initial
                })))

(defn- on-note [instrument osc-msg]
  (let [[note vel] (vec (. osc-msg (get_args)))]
    (swap! s assoc-in [instrument :notes note] vel)
    (log (str note " " vel))))

(listen "/keystation/note" (fn [osc-msg] (on-note :keystation osc-msg)))

(defn get-notes
  "returns map of currently played notes and their velocities"
  ([instrument]
   (->> (get-in @s [instrument :notes])
        (filter #(> (second %) 0))
        (flatten)
        (apply hash-map)))
  ([] (get-notes :default)))

(get-notes :keystation)


(defn get-note "returns current velocity of note"
  ([instrument n] (get-in @s [instrument :notes n]))
  ([n] (get-note :default)))

(get-note :keystation 50)

(@s :keystation)