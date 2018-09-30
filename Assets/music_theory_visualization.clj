(ns music-theory-visualization
  (:use [arcadia.core]
        [arcadia.introspection]
        [arcadia.linear]
        [music-instrument-state :only [get-notes listen]])
  (:import (UnityEngine GameObject Material Mathf Renderer Resources)))

(def mats (->> (range 12)
               (map (fn [n] [n (Resources/Load (str "n" n))]))
               (cons [:node (Resources/Load "node")])
               (flatten)
               (apply hash-map)))

(def spiral-obj (object-named "Spiral"))
(def note-size-min 0.31)
(def note-size-max 0.55)
(defn note->scale [n] (+ 1 (/ (/ n -12) 12)))
(def seg (/ Mathf/PI 6))
(def spiral
  (->> (range 21 109)
       (map (fn [n] (let [pos (mod n 12)
                          y (/ n -12)
                          scale (+ 1 (/ y 12))
                          x (* (Mathf/Sin (* seg n)) scale)
                          z (* (Mathf/Cos (* seg n)) scale)
                          node-name (str "node-" n)
                          note-name (str "note-" n)
                          node (create-primitive :sphere node-name)
                          note (create-primitive :sphere note-name)
                          ]
                      (child+ spiral-obj node)
                      (child+ spiral-obj note)
                      (set! (.. node transform localPosition) (v3 x y z))
                      (set! (.. note transform localPosition) (v3 x y z))
                      (set! (.. node transform localScale) (v3 (* note-size-min 0.9 scale)))
                      (set! (.. note transform localScale) (v3 (* 0.1 scale)))
                      (set! (.. (cmpt node Renderer) material) (mats :node))
                      (set! (.. (cmpt note Renderer) material) (mats pos))
                      [(keyword node-name) node
                       (keyword note-name) note]
                      )))
       (flatten)
       (apply hash-map)))


(defn- on-evt [device-name evt index val]
  (cond (= evt :note)
        (let [go (spiral (keyword (str "note-" index)))
              scale (note->scale index)]
          (set! (.. go transform localScale)
                (v3 (* scale
                       (Mathf/Lerp
                         (if (= 0 val) 0.1 note-size-min)
                         note-size-max
                         (/ val 128)))))
          )))

(defn- on-midi-evt [device-name]
  (fn [evt index val]
    (on-evt device-name evt index val)))

;(listen :keystation #'on-keystation-evt)
(listen :keystation (on-midi-evt :keystation))
;(listen :a-300 (on-midi-evt :a-300))