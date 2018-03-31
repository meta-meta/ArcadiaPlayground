(ns theremin
  (:use [arcadia.core]
        [arcadia.linear]
        [osc :only [send]])
  (:import (UnityEngine GameObject Mathf Vector3)))

(def left-hand (object-named "Hand - Left"))
(def right-hand (object-named "Hand - Right"))
(def amp-obj (object-named "Amplitude"))
(def freq-obj (object-named "Frequency"))

(def sixense-input (cmpt (object-named "SixenseInput") "SixenseInput"))
(.. sixense-input Controllers)

(defn is-docked []
  (< 0
     (->> (SixenseInput/Controllers)
          (map #(. % Docked))
          (filter identity)
          (count))))

(defn get-pos [^GameObject go] (.. go transform position))
(. (v3 1 1 1) sqrMagnitude)

(v3) (get-pos amp-obj)


(defn send-state []
  (send "/theremin/amp" (if (is-docked) 0
                                        (Vector3/Distance (get-pos amp-obj) (get-pos left-hand))))
  (send "/theremin/freq" (if (is-docked) 0
                                         (Vector3/Distance (get-pos freq-obj) (get-pos right-hand)))))



(defn game-loop [obj key]
  (send-state)
  )

(hook+ (object-named "App") :update :theremin #'game-loop)