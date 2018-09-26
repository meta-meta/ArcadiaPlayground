(ns studio
  (:use [arcadia.core]
        [arcadia.introspection]
        [arcadia.linear]
        [music-instrument-state :only [get-notes listen]])
  (:import (UnityEngine GameObject Material Renderer Resources)))

(def notes-in-c                                             ;TODO: import from music-notation
  (->> (cycle [2 2 1 2 2 2 1])
       (reductions + (- (mod 0 12) 12))
       (drop-while #(< % 0))
       (take-while #(< % 128))))

(def notes-in-c-set (set notes-in-c))

(def keystation-obj (object-named "Keystation"))

(def mat-black (Resources/Load "black-keys"))
(def mat-white (Resources/Load "white-keys"))

(defn- key+ [parent x-offset w note]
  (let [pivot (GameObject. (str "pivot-" note))
        key (create-primitive :cube "key")
        [h d] [0.03 0.155]
        mat (if (contains? notes-in-c-set note)
              mat-white
              mat-black)]
    (child+ pivot key)
    (child+ parent pivot)
    (set! (.. pivot transform localPosition) (v3 (* w x-offset) 0 0))
    (set! (.. key transform localPosition) (v3 0 0 (- (/ d 2))))
    (set! (.. key transform localScale) (v3 (* 0.95 w) h d))
    (set! (.. (cmpt key Renderer) material) mat)
    pivot))

(defn- index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

(defn- notes-in-c-range [low high]
  (filter #(and (>= % low) (<= % high)) notes-in-c))

(def keystation-keys
  (let [low 21
        high 108
        white-notes (notes-in-c-range low high)
        black-notes (filter #(not (contains? notes-in-c-set %)) (range low (+ 1 high)))
        idx-of-middle-c (index-of 60 white-notes)
        key-width (/ 1.225 (count white-notes))

        white-keys
        (->> white-notes
             (map-indexed
               (fn [index note]
                 [note
                  (key+ keystation-obj                      ;parent
                        (- index idx-of-middle-c)           ;x-offset
                        key-width                           ;width
                        note)]))                            ;note
             )

        black-keys
        (->> black-notes
             (map-indexed
               (fn [index note]
                 [note
                  (let [pivot (key+ keystation-obj             ;parent
                                 (- (index-of (+ note 1) white-notes)
                                    idx-of-middle-c
                                    0.5)                    ;x-offset
                                 key-width                  ;width
                                 note                       ;note
                                 )
                        key (first (children pivot))]
                    (.Translate (.. key transform) (v3 0 (/ key-width 2) 0.02))
                    (set! (.. key transform localScale)
                          (v3 (/ key-width 2) key-width 0.1))
                    pivot)])))
        ]

    (->> (concat white-keys black-keys)
         (flatten)
         (apply hash-map))
    ))

(defn- on-keystation-evt [evt index val]
  (cond (= evt :note)
        (let [go (keystation-keys index)]
          (when-not (nil? go)
            (set! (.. go transform localRotation)
                  (euler (v3 (if (= val 0) 0 -4) 0 0))))))
  (log "got " evt " " index " " val))

(listen :keystation #'on-keystation-evt)



