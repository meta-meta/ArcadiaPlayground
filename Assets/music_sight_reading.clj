(ns music-sight-reading
  (:use [arcadia.core]
        [arcadia.linear]
        [music-instrument-state :only [get-notes]]
        [music-live-compose]
        [music-notation :only [clear-notes
                               set-played-notes!
                               set-keysig!
                               +note
                               -note]]
        [scheduler :only [now start clear-queue queue+ elapsed]]
        )
  (:import (UnityEngine Color GameObject Mathf TextMesh)))


(defn game-loop [obj key]
  (set-played-notes! (get-notes)))

(hook+ (object-named "App") :update :sight-reading #'game-loop)

(defn scroll-go "scrolls :note event on staff by x"
  [^GameObject go x]
  (set! (.. go transform position)
        (v3 (+ music-notation/x0 (* 3 x)) 0 -0.1)))

(defn set-go-color "sets color of :note event"
  [^GameObject go r g b a]
  (doseq [child (children go)]
    (set! (. (cmpt child TextMesh) color)
          (Color. r g b a))))

(defn update-queued "invoked on every :queued event every frame"
  [note go t0]
  (let [seconds-left (- t0 (elapsed))]
    (scroll-go go seconds-left)))

(defn update-active "invoked on every :active event every frame"
  [note go t0]
  (let [[r g b] (if
                  (contains? (get-notes) note)
                  [0 1 0]
                  [1 0 0])
        secs-til-active (- t0 (elapsed))]
    (set-go-color go r g b (+ 1 secs-til-active))
    (scroll-go go (+ secs-til-active
                     (Mathf/Pow secs-til-active 2)))))



(defn queue-event
  "queues an event"
  [t note duration]
  (let [go (+note note t)
        t0 (+ (elapsed) t)]
    (queue+ {
             :t0            t0
             :duration      1
             :start         #()
             :end           #(-note go)
             :update-queued #(update-queued note go t0)
             :update-active #(update-active note go t0)
             }))
  nil)


(defn clear []
  (clear-notes)
  (clear-queue))


; TODO: put all notes in a sequence as children of container object, scroll container instead of individual notes
;(doseq [glyph-go (filter identity (flatten glyph-gos))]
;  (child+ go glyph-go true))

(comment

  (set-keysig! :c)

  (doall (map (fn [n t] (queue-event (+ 1 t) n 1))
              (concat
                ;(range-exercise-diatonic 60 34 99 1)
                ;(range-exercise-diatonic 60 34 99 2 0)
                ;(range-exercise-diatonic 60 34 99 2 1)
                (range-exercise-diatonic 60 34 99 3 0)
                (range-exercise-diatonic 60 34 99 3 1)
                (range-exercise-diatonic 60 34 99 3 2)
                (range-exercise-diatonic 60 34 99 4 0)
                (range-exercise-diatonic 60 34 99 4 1)
                (range-exercise-diatonic 60 34 99 4 2)
                (range-exercise-diatonic 60 34 99 4 3)
                )
              (reductions + (clojure.core/repeat 0.75))))

  (clear)

  (set-keysig! :db)

  )



