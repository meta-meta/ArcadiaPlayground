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
           :start    (if (= note :r) #()
                                     #(send-note note 63))
           :end      (if (= note :r) #()
                                     #(send-note note 0))
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


(defn queue-fn [t0 fn]
  (queue+ {
           :t0       (+ (elapsed) t0)
           :duration 1
           :start    fn
           :end      #()
           })
  nil)

(def notes-all (into (sorted-set) (range 0 128)))

(defn diatonic-scale [tonic] (reductions + tonic [2 2 1 2 2 2 1]))

(defn bpm->dur [bpm]
  (/ 60 bpm))

(defn pattern "queues pattern, returns duration of pattern"
  [notes rhythm bpm]
  (let [durations (map #(* % (bpm->dur bpm)) rhythm)
        start-times (take (count durations)
                          (cons 0 (reductions + durations)))]
    (doall (map
             (fn [note t dur] (queue-note t dur note))
             notes
             start-times
             (map #(* 0.95 %) durations)
             ))
    (reduce + durations)))

;TODO: allow full range
(defn diatonic-pattern [root pattern]
  (let [s (diatonic-scale root)]
    (map #(if (= % :r) :r
                       (nth s %))
         pattern)))

(defn repeat [pattern-fn]
  (let [dur (pattern-fn)]
    (queue-fn dur #(repeat pattern-fn))))

(defn repeater [dia-root dia-seq rhythm bpm]
  (pattern
    (diatonic-pattern dia-root dia-seq)
    rhythm
    bpm))

(defn re [n x] (clojure.core/repeat n x))

(comment
  (do ; Eight
    (def bpm 241)

    (defn five []
      (repeater 74
                [7 6 1 4 2]
                [1 1 1 1 2]
                bpm))

    (defn four []
      (repeater 62
                [0 2 0 2 :r]
                [1 1 1 1 1]
                bpm))

    (defn one []
      (repeater 62
                [5 :r]
                [2/3 3/3]
                bpm))

    (defn three-long []
      (repeater 62
                [5 :r 6 :r 7 :r :r]
                [20 8 20 8 20 24]
                bpm))

    (defn three-short []
      (repeater 74
                [:r 1 :r 4 :r 2]
                [1/2 7/2 1/2 7/2 1/2 7/2]
                bpm))

    (defn drone []
      (let [dur (* 10 (bpm->dur bpm))]
        (queue-chord 0 dur [50 57])
        dur))

    (do
      (repeat #(drone))
      (repeat #(one))
      (repeat #(four))
      (repeat #(five))
      (repeat #(three-long))
      ;(repeat #(three-short))
      )
    ) ;Eight
  (clear-queue)


  (do ; Eleven
    (def bpm 136)


    (defn eight-seven []
      (repeater 29
                (flatten (concat (re 8 [7 :r])
                                 (re 7 [5 :r])))
                (re 30 1/2)
                bpm))
    (defn four []
      (repeater 65
                [0 2 4 5 :r]
                (re 5 1/2)
                bpm))

    (defn six []
      (repeater 53
                [6 4 2 0 2 4]
                (re 6 1/3)
                bpm))

    (do
      (repeat #(four))
      (repeat #(six))
      (repeat #(eight-seven))
      )
    ) ;Eleven
  (clear-queue)

  (do ; Six
    (def bpm 88)

    (defn drone []
      (let [dur (* 10 (bpm->dur bpm))]
        (queue-chord 0 dur [46])
        dur))

    (defn five []
      (repeater 53
                [3 4 7 4 0]
                (re 5 1)
                bpm))

    (defn five-rest []
      (repeater 77
                (concat (flatten (re 5 [0 :r])) [:r])
                (concat (flatten (re 5 [3/5 1/5])) [4/5])
                bpm))

    (defn four []
      (repeater 70
                [2 3 4 0]
                [5/3 5/3 5/3 20/3]
                bpm))

    (do
      (repeat #(drone))
      (repeat #(four))
      (repeat #(five))
      (repeat #(five-rest))
      )
    ) ;Six
  (clear-queue)
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
  (start)
  )


