(ns music-sight-reading
  (:use [arcadia.core]
        [music-instrument-state :only [get-notes]]
        ;[music-live-compose]
        [music-notation :only [set-played-notes!
                               set-keysig!]]
        ))


(defn game-loop [obj key]
  (set-played-notes! (get-notes))
  )

(hook+ (object-named "App") :update :sight-reading #'game-loop)

(set-keysig! :bb) 