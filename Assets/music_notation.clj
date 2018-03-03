(ns music-notation
  (use arcadia.core
       arcadia.linear
       clojure.set)
  (:use [music-glyphs :only [glyphs]])
  (:import (UnityEngine Color GameObject Mesh MeshFilter MeshRenderer Mathf TextMesh Vector2 Vector3))
  )

(def lexicon "examples and definitions of terms used"
  {
   :note-value "1/2 - the rhythmic length of a note event"
   :note "60 - the MIDI number corresponding to pitch"
   :pitch ":c4 - a keyword corresponding to pitch"
   :pitch-class ":c - a keyword corresponding to pitch class with no octave"
   :glyph ":note-2-up - a keyword specifying the font character to render"
   })

; Trying to make the glyph from scratch but seems font won't load dynamically
;(let [obj (GameObject. "abc")
;      tm (cmpt+ obj UnityEngine.TextMesh)]
;  (set! (.. obj transform localScale) (v3 0.2))
;  (set! (. tm fontSize) (int 100))
;  (set! (. tm font) (UnityEngine.Resources/Load "Bravura"))
;  (set! (. tm text) (str \uE050))
;  )

(def dx "smallest unit in x-dimension for rendering glyphs" 1)
(def dy "smallest unit in y-dimension for rendering glyphs" 0.25)

(defn +glyph
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

(defn +staff [n y]
  (doall (->> (range (* 3 n))
              (map #(+glyph :staff-5 % y)))))

(defn +note
  "creates a note on the staff with optionally accidental and dot"
  [t y note-value accidental dotted]
  (let [x0 (* 3 t)]
    (when accidental (+glyph accidental x0 y))
    (+glyph
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
    (when dotted (+glyph :note-dot (+ 2 x0) y))))


(defn note->pitch-class-b-bias
  "returns pitch class given note, with flats as accidentals"
  [note]
  (nth (cycle [:c :db :d :eb :e :f :gb :g :ab :a :bb :b]) note))

(defn note->pitch-class-#-bias
  "returns pitch class given note, with sharps as accidentals"
  [note]
  (nth (cycle [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b]) note))

(defn octave-of-note
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

(def notes-in-c
  "a set of all notes in key of c"
  (set (notes-in-key 0)))

(defn strip-accidental
  "returns the natural pitch class of provided note or pitch-class"
  [pitch-class]
  (keyword (subs (name pitch-class) 0 1)))

(defn extract-accidental
  "returns the accidental or nil"
  [pitch-class]
  (if (< (count (name pitch-class)) 2)
    nil
    (keyword (subs (name pitch-class) 1 2))))





(def keys-data ; TODO cb and c# ??
  ; to get the accidentals in each key, we walk in either direction on the circle of 5ths
  (->> [:sharp :flat]
       (map
         (fn [bias]
           (let [
                 [note->pitch-class interval]
                 (case bias ;either prefer sharp or flat when spelling notes
                   :flat [note->pitch-class-b-bias 5] ; prefer flats, walk by 4ths
                   :sharp [note->pitch-class-#-bias 7]) ; prefer sharps, walk by 5ths

                 circle-order ; order of notes around the circle of 5ths
                 (->> (range 12)
                      (map #(mod (* interval %) 12))
                      (map note->pitch-class))
                 ]
             (->> (range 7) ; 7 keys in each direction (c is duplicated then deduped later)
                  (map #(mod (* interval %) 12))
                  (map (fn [note]
                         (let [key (note->pitch-class note)

                               accidentals
                               (->> (notes-in-key note) ;all notes in this note's key
                                    (take 7) ; there are only 7 pitch classes in a key
                                    (map #(when-not ; a note not in the key of c is an accidental
                                            (contains? notes-in-c %)
                                            (note->pitch-class %))) ; get its pitch-class
                                    (filter identity) ; filter nil (non-accidentals)
                                    (set) ; it's not in the order we want. more useful as a set
                                    )

                               accidentals-renamed
                               (case key ; when the key...
                                 :f# (union #{:e#} accidentals) ; add :e#
                                 :gb (union #{:cb} accidentals) ; add :cb
                                 accidentals ; otherwise leave it alone
                                 )

                               circle-order-renamed
                               (->> circle-order
                                    (map #(cond
                                            (and (= key :f#) (= % :f)) :e#
                                            (and (= key :gb) (= % :b)) :cb
                                            true %)))

                               accidentals-ordered
                               (->> circle-order-renamed
                                    (filter #(contains? accidentals-renamed %)))

                               note-spellings
                               (->> (range 128)
                                    (map #(let [
                                                pc
                                                (note->pitch-class %)

                                                spelling
                                                (cond
                                                  ; corrections for :f# and :gb
                                                  (and (= key :f#) (= pc :e))
                                                  :en

                                                  (and (= key :f#) (= pc :f))
                                                  :e

                                                  (and (= key :gb) (= pc :c))
                                                  :cn

                                                  (and (= key :gb) (= pc :b))
                                                  :c

                                                  ; accidentals are spelled without accidental
                                                  (contains? accidentals pc)
                                                  (strip-accidental pc)

                                                  ; base pc of accidentals are spelled "natural"
                                                  (contains?
                                                    (set (map strip-accidental accidentals))
                                                    pc)
                                                  (keyword (str (name (strip-accidental pc))
                                                                "n"))

                                                  ;otherwise leave it alone
                                                  true pc)

                                                octave
                                                (octave-of-note %)
                                                ]
                                            {
                                             :pc  (strip-accidental spelling)
                                             :acc (extract-accidental spelling)
                                             :oct octave
                                             })))
                               ]
                           {
                            :key         key
                            :note        note
                            :accidentals accidentals-ordered
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

(defn create-grand-staff []
  (+glyph :staff-5 -2 2)
  (+glyph :staff-5 -1 2)
  (+glyph :clef-g -2 4)

  (+glyph :staff-5 -2 -10)
  (+glyph :staff-5 -1 -10)
  (+glyph :clef-f -2 -4)

  (+staff 10 2)
  (+staff 10 -10))

(create-grand-staff)


(def pitch->staff-y
  (->> (notes-in-key 0)
       (map-indexed
         #(apply vector [
                         (let [{pc :pc oct :oct} (spell-note :c %2)]
                           (keyword (str (last (str pc)) oct)))
                         (- %1 35) ; :c4 = 60 = index 35 of notes in key of c
                         ]))
       (flatten)
       (apply hash-map)
       ))


(defn +keysig
  "creates the glyphs of the given key signature within a vertical range on the staff"
  [key min-staff-y max-staff-y]
  (doall
    (->> (:accidentals (key keys-data))
         (map-indexed #(let [pitch-class (nth (str %2) 1)
                             y (->> (range 6)
                                    (map (fn [o] (pitch->staff-y (keyword (str pitch-class o)))))
                                    (filter (fn [y] (and (>= y min-staff-y)
                                                         (<= y max-staff-y))))
                                    (first))
                             x %1
                             accidental (case (last (str %2))
                                          \# :sharp
                                          \b :flat)]
                         (+glyph accidental x y))))))

(defn +keysig-treble [key]
  (+keysig key 4 12))
(defn +keysig-bass [key]
  (+keysig key -10 -3))

(+keysig-treble :gb)
(+keysig-bass :gb)


(comment

  (spell-note :f# 60)

  (:accidentals (:cb keys-data))

  (do
    (+glyph :ledger 2 0)
    (+glyph :ledger 2 12)

    (+glyph :flat 0 0)
    (+glyph :note-1 1 0)

    (+note 3 2 1/2 :flat true)

    (->> (range 10)
         (map #(+note % (- % 5) 1 :flat false)))

    (->> (range 10)
         (map #(+note % (+ % 5) 1 :sharp false)))

    (+glyph :flat 2 1)
    (+glyph :note-1 3 1)
    ))

