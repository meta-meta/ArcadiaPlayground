(ns music-sight-reading
  (:use [arcadia.core]
        [arcadia.linear]
        [music-instrument-state :only [get-notes]]
        [music-live-compose]
        [music-notation :only [set-played-notes!
                               set-keysig!
                               +note
                               -note]]
        [scheduler :only [now start clear-queue queue+ elapsed evt-seconds-left]]
        )
  (:import (UnityEngine Color TextMesh)))


(defn game-loop [obj key]
  (set-played-notes! (get-notes))
  )

(hook+ (object-named "App") :update :sight-reading #'game-loop)

(defn move-go [go t0 rate]
  (let [seconds-left (- t0 (elapsed))]
    (set! (.. go transform position)
          (v3 (+ music-notation/x0 (* rate (* 3 seconds-left))) 0 0))))

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
             :update-queued #(move-go go t0 1)

             :update-active (fn []
                              (doseq [child (children go)]
                                (let [tm (cmpt child TextMesh)
                                      playing? (contains? (get-notes) note)]
                                  (set! (. tm color) (if playing?
                                                       (Color. 0. 1. 0. 0.6)
                                                       (Color. 1. 0. 0. 0.6)))))
                              (move-go go t0 1/2)
                              )
             }))
  nil)


(doall (map (fn [n t] (queue-event (+ 5 t) n 1))
            (range-exercise-diatonic 53 48 72 3)
            (reductions + (clojure.core/repeat 1))))



(set-keysig! :c)