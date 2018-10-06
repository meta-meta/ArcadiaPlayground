(ns functional-objects
  (:use [arcadia.core]
        [arcadia.linear]
        [arcadia.introspection]
        ))

(def s (atom {
              :nodes #{
                       {
                        :name "bang"
                        :go   (object-named "go-74902")
                        :ins  [(fn [outs gui] ; an in is a fn of outs (and gui side-effect). no other args. it's a bang
                                 (doseq [out [outs]] ;for each out
                                   (doseq [conn (get-conns-from-out out)] ;for each conn of each out
                                     ((get-in-from-connection conn)))) ;execute the in of the conn
                                 )]
                        :outs [{:connections [{:go "go-8948" :in 1} {:go "go-3986" :in 0}]}] ; 1 out, connected to mult objects
                        :gui (fn (comment "light up"))
                        }
                       }

              }))
;TODO separate object type from its state. type {:name :ins :outs :gui}  connections {