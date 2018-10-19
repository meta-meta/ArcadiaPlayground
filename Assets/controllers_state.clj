(ns controllers-state
  (:require [osc :as o]
            [app-state :as app])
  (:use [arcadia.core]
        [arcadia.introspection]
        [arcadia.linear]
        [clojure.set :only [difference union]]
        [clojure.reflect]
        [clojure.pprint])
  (:import
    OVRInput
    (SpaceNavigatorDriver Settings SpaceNavigator)
    (UnityEngine ForceMode Input KeyCode Rigidbody)))

(def ovr-consts
  {
   :button-none                  OVRInput+Button/None
   :button-one                   OVRInput+Button/One
   :button-two                   OVRInput+Button/Two
   :button-three                 OVRInput+Button/Three
   :button-four                  OVRInput+Button/Four
   :button-start                 OVRInput+Button/Start
   :button-back                  OVRInput+Button/Back
   :button-pri-shoulder          OVRInput+Button/PrimaryShoulder
   :button-pri-index-trigger     OVRInput+Button/PrimaryIndexTrigger
   :button-pri-hand-trigger      OVRInput+Button/PrimaryHandTrigger
   :button-pri-thumbstick        OVRInput+Button/PrimaryThumbstick
   :button-pri-thumbstick-up     OVRInput+Button/PrimaryThumbstickUp
   :button-pri-thumbstick-down   OVRInput+Button/PrimaryThumbstickDown
   :button-pri-thumbstick-left   OVRInput+Button/PrimaryThumbstickLeft
   :button-pri-thumbstick-right  OVRInput+Button/PrimaryThumbstickRight
   :button-pri-touchpad          OVRInput+Button/PrimaryTouchpad
   :button-sec-shoulder          OVRInput+Button/SecondaryShoulder
   :button-sec-index-trigger     OVRInput+Button/SecondaryIndexTrigger
   :button-sec-hand-trigger      OVRInput+Button/SecondaryHandTrigger
   :button-sec-thumbstick        OVRInput+Button/SecondaryThumbstick
   :button-sec-thumbstick-up     OVRInput+Button/SecondaryThumbstickUp
   :button-sec-thumbstick-down   OVRInput+Button/SecondaryThumbstickDown
   :button-sec-thumbstick-left   OVRInput+Button/SecondaryThumbstickLeft
   :button-sec-thumbstick-right  OVRInput+Button/SecondaryThumbstickRight
   :button-sec-touchpad          OVRInput+Button/SecondaryTouchpad
   :button-dpad-up               OVRInput+Button/DpadUp
   :button-dpad-down             OVRInput+Button/DpadDown
   :button-dpad-left             OVRInput+Button/DpadLeft
   :button-dpad-right            OVRInput+Button/DpadRight
   :button-up                    OVRInput+Button/Up
   :button-down                  OVRInput+Button/Down
   :button-left                  OVRInput+Button/Left
   :button-right                 OVRInput+Button/Right
   :button-any                   OVRInput+Button/Any

   :touch-none                   OVRInput+Touch/None
   :touch-one                    OVRInput+Touch/One
   :touch-two                    OVRInput+Touch/Two
   :touch-three                  OVRInput+Touch/Three
   :touch-four                   OVRInput+Touch/Four
   :touch-pri-index-trigger      OVRInput+Touch/PrimaryIndexTrigger
   :touch-pri-thumbstick         OVRInput+Touch/PrimaryThumbstick
   :touch-pri-thumb-rest         OVRInput+Touch/PrimaryThumbRest
   :touch-pri-touchpad           OVRInput+Touch/PrimaryTouchpad
   :touch-sec-index-trigger      OVRInput+Touch/SecondaryIndexTrigger
   :touch-sec-thumbstick         OVRInput+Touch/SecondaryThumbstick
   :touch-sec-thumb-rest         OVRInput+Touch/SecondaryThumbRest
   :touch-sec-touchpad           OVRInput+Touch/SecondaryTouchpad
   :touch-any                    OVRInput+Touch/Any

   :near-touch-none              OVRInput+NearTouch/None
   :near-touch-pri-index-trigger OVRInput+NearTouch/PrimaryIndexTrigger
   :near-touch-pri-thumb-buttons OVRInput+NearTouch/PrimaryThumbButtons
   :near-touch-sec-index-trigger OVRInput+NearTouch/SecondaryIndexTrigger
   :near-touch-sec-thumb-buttons OVRInput+NearTouch/SecondaryThumbButtons
   :near-touch-any               OVRInput+NearTouch/Any

   :axis-1d-none                 OVRInput+Axis1D/None
   :axis-1d-pri-index-trigger    OVRInput+Axis1D/PrimaryIndexTrigger
   :axis-1d-pri-hand-trigger     OVRInput+Axis1D/PrimaryHandTrigger
   :axis-1d-sec-index-trigger    OVRInput+Axis1D/SecondaryIndexTrigger
   :axis-1d-sec-hand-trigger     OVRInput+Axis1D/SecondaryHandTrigger
   :axis-1d-any                  OVRInput+Axis1D/Any

   :axis-2d-none                 OVRInput+Axis2D/None
   :axis-2d-pri-thumbstick       OVRInput+Axis2D/PrimaryThumbstick
   :axis-2d-pri-touchpad         OVRInput+Axis2D/PrimaryTouchpad
   :axis-2d-sec-thumbstick       OVRInput+Axis2D/SecondaryThumbstick
   :axis-2d-sec-touchpad         OVRInput+Axis2D/SecondaryTouchpad
   :axis-2d-any                  OVRInput+Axis2D/Any

   :controller-none              OVRInput+Controller/None
   :controller-l-touch           OVRInput+Controller/LTouch
   :controller-r-touch           OVRInput+Controller/RTouch
   :controller-touch             OVRInput+Controller/Touch
   :controller-remote            OVRInput+Controller/Remote
   :controller-gamepad           OVRInput+Controller/Gamepad
   :controller-touchpad          OVRInput+Controller/Touchpad
   :controller-l-tracked-remote  OVRInput+Controller/LTrackedRemote
   :controller-r-tracked-remote  OVRInput+Controller/RTrackedRemote
   :controller-active            OVRInput+Controller/Active
   :controller-all               OVRInput+Controller/All
   })

(comment
  (OVRInput/Get (:axis-1d-pri-index-trigger ovr-consts) (:controller-l-touch ovr-consts))
  (OVRInput/Get (:button-any ovr-consts) (:controller-l-touch ovr-consts))
  )


; TODO find me a better home
(SpaceNavigator/SetRotationSensitivity 1)
(SpaceNavigator/SetTranslationSensitivity 0.5)
(set! Settings/RuntimeEditorNav false)

; Make sure "Filter Duplicates" is unchecked in OscIn component

(def s (let [blank-map (fn [n] (->> (range n)
                                    (map #(vector % 0))
                                    (flatten)
                                    (apply hash-map)))
             inst-initial {
                           :cc        (blank-map 128)
                           :notes     (blank-map 128)
                           :listeners #{}
                           }]
         {
          :default        inst-initial
          :a-300          inst-initial
          :bcr-2000       {
                           :knobs   (blank-map 55)
                           :buttons (blank-map 19)
                           }
          :keystation     inst-initial
          :acoustic-pitch {
                           :listeners #{}
                           :note      [0.0 0.0]
                           }

          :spacenav       {
                           :listeners   #{}
                           :rotation    (qt)
                           :translation (v3 0)
                           }

          :bike           {
                           :listeners #{}
                           :rpm       0.0
                           }
          }))

(swap! app/s assoc :controllers s)





; BIKE
(def cube (object-named "Cube"))
(def avatar (object-named "MyAvatar"))
(def handlebar (object-named "Handlebar"))

(defn- move-bike []

  (.AddForce (cmpt cube Rigidbody)
             (v3* (.. handlebar transform forward)
                  (get-in @app/s [:controllers :bike :rpm]) 0.1)
             ForceMode/Acceleration)

  )




(defn- on-midi-evt [instrument event osc-msg]
  (let [[index val] (vec (. osc-msg (get_args)))
        listeners (get-in @app/s [instrument :listeners])]
    (swap! app/s assoc-in [:controllers instrument event index] val)
    (doseq [listener listeners] (listener event index val))
    ))

(defn- on-pitch-evt [instrument osc-msg]
  (let [pitch-and-amp (vec (. osc-msg (get_args)))
        listeners (get-in @app/s [instrument :listeners])]
    (swap! app/s assoc-in [:controllers instrument :note] pitch-and-amp)
    (doseq [listener listeners] (listener pitch-and-amp))
    ))

(defn- on-rpm-evt [osc-msg]
  (let [[rpm] (vec (. osc-msg (get_args)))
        listeners (get-in @app/s [:bike :rpm :listeners])]
    (swap! app/s assoc-in [:controllers :bike :rpm] rpm)
    (doseq [listener listeners] (listener rpm))
    ))

(defn update-spacenav []
  (let [spacenav {
                  :translation (.. SpaceNavigator Translation)
                  :rotation    (.. SpaceNavigator Rotation)}
        listeners (get-in @app/s [:spacenav :listeners])]
    (swap! app/s update-in [:controllers :spacenav] #(union % spacenav))
    (doseq [listener listeners] (listener spacenav))
    )
  )

(defn poll [obj key]
  (update-spacenav)
  (move-bike)
  ;(swap! s assoc :keys {
  ;                      :space (Input/GetKey (. KeyCode Space))
  ;                      :a (Input/GetKey (. KeyCode A))
  ;                      :b (Input/GetKey (. KeyCode B))
  ;                      :c (Input/GetKey (. KeyCode C))
  ;                      })
  )

(hook+ (object-named "App") :update #'poll)

(o/listen "/a-300/note" (fn [osc-msg] (on-midi-evt :a-300 :note osc-msg)))
(o/listen "/keystation/note" (fn [osc-msg] (on-midi-evt :keystation :note osc-msg)))
(o/listen "/bcr-2000/buttons" (fn [osc-msg] (on-midi-evt :bcr-2000 :buttons osc-msg)))
(o/listen "/bcr-2000/knobs" (fn [osc-msg] (on-midi-evt :bcr-2000 :knobs osc-msg)))
(o/listen "/acoustic-pitch/note" (fn [osc-msg] (on-pitch-evt :acoustic-pitch osc-msg)))

(o/listen "/rpm" (fn [osc-msg] (on-rpm-evt osc-msg)))

(defn listen
  "registers a listener for instrument events. listener must accept args: midi-evt index val"
  [instrument listener]
  (log (str instrument listener))
  (swap! app/s update-in [:controllers instrument :listeners] #(union % #{listener}))
  nil
  )

(defn get-notes
  "returns map of currently played notes and their velocities"
  ([instrument]
   (->> (get-in @app/s [:controllers instrument :notes])
        (filter #(> (second %) 0))
        (flatten)
        (apply hash-map)))
  ([] (get-notes :default)))

(defn get-note "returns current velocity of note"
  ([instrument n] (get-in @app/s [:controllers instrument :notes n]))
  ([n] (get-note :default)))