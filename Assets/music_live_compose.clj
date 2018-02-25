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

(defn diatonic-scale-cycle-up
  "lazy seq of diatonic scale starting at tonic"
  [tonic]
  (reductions + tonic
              (cycle [2 2 1 2 2 2 1])))

(defn diatonic-scale-cycle-dn
  "lazy seq of diatonic scale, descending starting at tonic"
  [tonic]
  (reductions - tonic
              (cycle (reverse [2 2 1 2 2 2 1]))))

(defn bpm->dur [bpm]
  (/ 60 bpm))

(defn pattern "queues pattern, returns duration of pattern"
  [notes rhythm bpm]
  (let [durations (map #(* % (bpm->dur bpm))
                       (take (count notes) rhythm))
        start-times (take (count durations)
                          (cons 0 (reductions + durations)))]
    (doall (map
             (fn [note t dur] (queue-note t dur note))
             notes
             start-times
             (map #(* 0.95 %) durations)
             ))
    (reduce + durations)))

(defn diatonic-pattern
  "returns seq of notes given a root and seq of scale degrees +/-"
  [root scale-degrees]
  (map (fn [sd]
         (if (= sd :r)
           :r
           (if (> sd 0)
             (nth (diatonic-scale-cycle-up root)
                  sd)
             (nth (diatonic-scale-cycle-dn root)
                  (- sd)))))
       scale-degrees))

(defn repeat-queue [pattern-fn]
  (let [dur (pattern-fn)]
    (queue-fn dur #(repeat-queue pattern-fn))))

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
      (repeat-queue #(drone))
      (repeat-queue #(one))
      (repeat-queue #(four))
      (repeat-queue #(five))
      (repeat-queue #(three-long))
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
      (repeat-queue #(four))
      (repeat-queue #(six))
      (repeat-queue #(eight-seven))
      )
    ) ;Eleven
  (clear-queue)

  (do ; Six
    (def bpm 88)

    (defn drone []
      (let [dur (* 10 (bpm->dur bpm))]
        (queue-chord 0 dur [46])
        dur))

    (defn five [] ;TODO: use actual tonic and -scale degrees
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
      (repeat-queue #(drone))
      (repeat-queue #(four))
      (repeat-queue #(five))
      (repeat-queue #(five-rest))
      )
    ) ;Six
  (clear-queue)
  )




(defn chord-cycle [steps]
  (doall (map (fn [n t] (queue-chord (/ t 1) 1/16 n))
              (let [scale (diatonic-scale-cycle-up 48)
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



(defn range-exercise-diatonic
  "generates a sequence of notes traversing the diatonic scale by steps within the range (low, high)"
  [key low high step]
  (let [r (range 0 128 step)]
    (dedupe (filter
              #(and (>= % low) (<= % high))
              (diatonic-pattern key
                                (concat r (reverse r)))))))

(defn queue-range-exercises-diatonic
  "queues range exercises cycling through keys and steps provided"
  [low high key-step-pairs bpm]
  (when-not (empty? key-step-pairs)
    (let [[key step] (first key-step-pairs)
          dur (pattern
                (range-exercise-diatonic key low high step)
                (clojure.core/repeat 1)
                bpm)]
      (queue-fn (+ 1 dur)
                #(queue-range-exercises-diatonic
                   low high (rest key-step-pairs) bpm)))))



(comment

  (let [pat (diatonic-pattern
              60
              (map #(int (* 15 (Mathf/Sin (/ % 10))))
                   (range 50)))]
    (pattern pat
             (re (count pat) 1)
             300))


  (let [keys-by-fourths (map #(mod (* 5 %) 12) (range 12))
        key-step-pairs (->> keys-by-fourths
                            (map (fn [key]
                                   (map #(vector %1 %2)
                                        (clojure.core/repeat key)
                                        (range 1 7))))
                            (apply concat))]
    (queue-range-exercises-diatonic 36 96 key-step-pairs 100))


  (chord-cycle 20)
  (clear-queue)
  (start)
  )


