(ns music-live-compose
  (:use [scheduler :only [start clear-queue queue+ elapsed]])
  (:use [osc :only [send]])
  (:import [UnityEngine.Mathf]))

(import UnityEngine.Mathf)

(defn send-note [note vel] (send "/organ" [note vel]))

(defn send-chord [notes vel]
  (doall (map (fn [note] (send-note note vel))
              notes)))

(comment
  (send-note 60 63)
  (send-note 60 0)
  (send-chord [60 64 68 74] 63)
  (send-chord [60 64 68 74] 0)
  )

(defn queue-note [t0 dur note]
  (queue+ {
           :t0       (+ (elapsed) t0)
           :duration dur
           :start    #(send-note note 63)
           :end      #(send-note note 0)
           })
  nil)

(defn queue-chord [t0 dur notes]
  (queue+ {
           :t0       (+ (elapsed) t0)
           :duration dur
           :start    #(send-chord notes 63)
           :end      #(send-chord notes 0)
           })
  nil)



(def notes-all (into (sorted-set) (range 0 128)))

(defn diatonic-scale [tonic] (reductions + tonic [2 2 1 2 2 2 1]))


(comment
  (queue-chord 0 1/16 [60 64 69])
  (queue-note 0 1/16 69)

  (map #(queue-note (/ (+ %1 (Mathf/Abs (Mathf/Sin (/ %1 8)))) 8) 1/64 %2)
       (range)
       [60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74 60 62 64 66 69 74])
  )

(defn chord-cycle [steps]
  (doall (map (fn [n t] (queue-chord (/ t 1) 1/16 n))
              (let [scale (cycle (diatonic-scale 48))
                    chord-degrees (map
                                    #(range % (+ % 5) 3)
                                    (range steps))
                    ]
                (map (fn [notes] (map #(nth scale %)
                                      notes))
                     chord-degrees)
                )
              (range steps)))
  nil)

(comment
  (chord-cycle 20)
  (clear-queue)
  )


(start)