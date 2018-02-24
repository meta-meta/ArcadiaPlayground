(ns osc
  (:use [arcadia.core]))

(def osc-out (cmpt (object-named "OSC") "OscOut"))

(. osc-out (Open 8000 "127.0.0.1"))

(defn send [addr msg]
  (let [msg (cond
              (coll? msg) (to-array (map int msg)) ; TODO other types in array
              (int? msg) (int msg)
              :else msg)]
    (. osc-out (Send addr msg))))

(send "/organ" [60 0])
(send "/drawbar" [0 60])
(send "/drawbar" [1 127])

(map #(send "/drawbar" [% (* 127 (- 1 (/ % 9)))]) (range 9))

