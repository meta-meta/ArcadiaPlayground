(ns studio
  (:use [arcadia.core]
        [arcadia.introspection]
        [arcadia.linear]
        [music-instrument-state :only [get-notes listen]])
  (:import (UnityEngine GameObject)))

(def keystation-obj (object-named "Keystation"))

(defn- key+ [parent x-offset w note]
  (let [pivot (GameObject. (str "pivot-" note))
        key (create-primitive :cube "key")
        [h d] [0.03 0.155]]
    (child+ pivot key)
    (child+ parent pivot)
    (set! (.. pivot transform localPosition) (v3 (* w x-offset) 0 0))
    (set! (.. key transform localPosition) (v3 0 0 (- (/ d 2))))
    (set! (.. key transform localScale) (v3 (* 0.95 w) h d))
    pivot))

(def notes-in-c                                             ;TODO: import from music-notation
  (->> (cycle [2 2 1 2 2 2 1])
       (reductions + (- (mod 0 12) 12))
       (drop-while #(< % 0))
       (take-while #(< % 128))))

(defn- index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

(defn- notes-in-c-range [low high]
  (filter #(and (>= % low) (<= % high)) notes-in-c))

(def keystation-keys
  (let [notes (notes-in-c-range 21 108)
        idx-of-middle-c (index-of 60 notes)]
    (->> notes
         (map-indexed (fn [index note]
                        [note
                         (key+ keystation-obj
                               (- index idx-of-middle-c)
                               (/ 1.225 (count notes))
                               note)]))
         (flatten)
         (apply hash-map))))

(defn- on-keystation-evt [evt index val]
  (cond (= evt :note)
        (let [go (keystation-keys index)]
          (log "note")
          (set! (.. go transform localRotation)
                (euler (v3 (if (= val 0) 0 -7) 0 0)))))
  (log "got " evt " " index " " val))


(listen :keystation #'on-keystation-evt)



