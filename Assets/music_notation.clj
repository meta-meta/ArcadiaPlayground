(ns music-notation
  (use arcadia.core
       arcadia.linear
       clojure.set)
  (:use [music-glyphs :only [glyphs]])
  (:import (UnityEngine Color GameObject Mesh MeshFilter MeshRenderer Mathf TextMesh Vector2 Vector3))
  )

; Trying to make the glyph from scratch but seems font won't load dynamically
;(let [obj (GameObject. "abc")
;      tm (cmpt+ obj UnityEngine.TextMesh)]
;  (set! (.. obj transform localScale) (v3 0.2))
;  (set! (. tm fontSize) (int 100))
;  (set! (. tm font) (UnityEngine.Resources/Load "Bravura"))
;  (set! (. tm text) (str \uE050))
;  )

(def dx 1)
(def dy 0.25)

(defn create-glyph
  "creates a glyph at (x,y,0). returns the GameObject"
  [glyph x y]
  (let [go (instantiate (object-named "GlyphTemplate"))
        tm (cmpt go TextMesh)]
    (set! (.. go transform position) (v3 (* x dx)
                                         (* y dy) ;center on middle c = 0
                                         0))
    (set! (. go name) (str glyph))
    (set! (. tm color) (Color. 0. 0. 0. 0.5))
    (set! (. tm text) (str (glyph glyphs)))
    go
    ))

(defn create-staff [n y]
  (->> (range (* 3 n))
       (map #(create-glyph :staff-5 % y))))

(defn create-evt [t y note-value accidental dotted]
  (let [x0 (* 3 t)]
    (when accidental (create-glyph accidental x0 y))
    (create-glyph
      (case note-value
        1 :note-1
        1/2 :note-2-up
        1/4 :note-4-up
        1/8 :note-8-up
        1/16 :note-16-up
        1/32 :note-32-up
        )
      (+ 1 x0) ; make room for accidental
      y
      )
    (when dotted (create-glyph :note-dot (+ 2 x0) y))))


(defn note->pitch-class-flats
  "returns pitch class given note, with flats as accidentals"
  [note]
  (nth (cycle [:c :db :d :eb :e :f :gb :g :ab :a :bb :b]) note))

(defn note->pitch-class-sharps
  "returns pitch class given note, with sharps as accidentals"
  [note]
  (nth (cycle [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b]) note))

(defn octave
  "returns the octave # of a given note see: https://en.wikipedia.org/wiki/Scientific_pitch_notation#Table_of_note_frequencies"
  [note]
  (- (int (/ note 12)) 1))

(defn notes-in-key
  "returns all notes in the major key of the given note"
  [note]
  (->> (cycle [2 2 1 2 2 2 1])
       (reductions + (- (mod note 12) 12))
       (drop-while #(< % 0))
       (take-while #(< % 128))))

(def notes-in-c (set (notes-in-key 0)))

(defn strip-accidental
  "returns the natural pitch class"
  [pitch-class]
  (keyword (str (nth (str pitch-class) 1))))

(defn extract-accidental
  "returns the accidental or nil"
  [pitch-class]
  (if (< (count (str pitch-class)) 3)
    nil
    (keyword (apply str (drop 2 (str pitch-class))))))

(defn note-data [natural-pitch-class accidental octave]
  {
   :pc natural-pitch-class
   :acc accidental
   :oct octave
   })

(def keys-data
  (->> [:sharp :flat] ; to get the accidentals in each key, we walk in either direction on the circle of 5ths
       (map (fn [bias]
              (let [
                    note->pitch-class (case bias ;either prefer sharp or flat when spelling notes
                                        :flat note->pitch-class-flats
                                        :sharp note->pitch-class-sharps)
                    interval (case bias ; which way do we walk the circle of 5ths?
                               :flat 5 ; walking by 4ths, flat
                               :sharp 7)] ;walking by 5ths, sharp
                (->> (range 7)
                     (map #(mod (* interval %) 12))
                     (map (fn [note]
                            (let [key (note->pitch-class note)
                                  accidentals (->> (notes-in-key note) ;all notes in this note's key
                                                   (take 7) ; there are only 7 pitch classes in a key
                                                   (map #(when-not ; a note not in the key of c is an accidental
                                                           (contains? notes-in-c %)
                                                           (note->pitch-class %))) ; get its pitch-class
                                                   (filter identity) ; filter nil (non-accidentals)
                                                   (set) ; it's not in the order we want. more useful as a set
                                                   )
                                  note-spellings
                                  (->> (range 128)
                                       (map #(let [pc (note->pitch-class %)
                                                   spelling (cond
                                                              (and (= key :f#) (= pc :e))
                                                              :en

                                                              (and (= key :f#) (= pc :f))
                                                              :e

                                                              (and (= key :gb) (= pc :c))
                                                              :cn

                                                              (and (= key :gb) (= pc :b))
                                                              :c

                                                              (contains? accidentals pc) ;accidentals
                                                              (strip-accidental pc) ; are spelled without accidental

                                                              (contains? ; base pc of accidentals
                                                                (set (map strip-accidental accidentals))
                                                                pc)
                                                              (keyword (str (last (str (strip-accidental pc))) "n")) ; are spelled "natural"

                                                              true ;otherwise
                                                              pc ; leave it alone
                                                              )
                                                   octave (octave %)]
                                               {
                                                :pc  (strip-accidental spelling)
                                                :acc (extract-accidental spelling)
                                                :oct octave
                                                })))
                                  ]
                              {
                               :key         key
                               :note        note
                               :accidentals (case key ; when the key...
                                              :f# (union #{:e#} accidentals) ; add :e#
                                              :gb (union #{:cb} accidentals) ; add :cb
                                              accidentals ; otherwise leave it alone
                                              ) ; TODO: put these in order - no need to look them up outside this
                               :spellings   (apply vector note-spellings) ; faster lookup as vector
                               })))))))
       (flatten) ; combine both sequences. the key of c will be present in both.
       (#(zipmap (map :key %) %)) ; turn it into a map so we can dedupe and look up data by key
       )
  )

(defn spell-note
  "returns map of note spelling data: {:pc :c, :acc nil, :oct 4}"
  [key note]
  (nth (:spellings (key keys-data)) note))

(defn note-spelling->y-and-accidental ":c#4 => {:y 0 :acc :sharp}" [spelling])

(comment

  (spell-note :c 60)

  (do

    (create-glyph :staff-5 -2 2)
    (create-glyph :staff-5 -1 2)
    (create-glyph :clef-g -2 4)

    (create-glyph :staff-5 -2 -10)
    (create-glyph :staff-5 -1 -10)
    (create-glyph :clef-f -2 -4)


    (create-glyph :ledger 2 0)
    (create-glyph :ledger 2 12)

    (create-glyph :flat 0 0)
    (create-glyph :note-1 1 0)

    (create-evt 3 2 1/2 :flat true)

    (->> (range 10)
         (map #(create-evt % (- % 5) 1 :flat false)))

    (->> (range 10)
         (map #(create-evt % (+ % 5) 1 :sharp false)))

    (create-glyph :flat 2 1)
    (create-glyph :note-1 3 1)

    (create-staff 10 2)
    (create-staff 10 -10)
    ))

