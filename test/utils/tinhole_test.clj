(ns utils.tinhole-test
  (:use clojure.walk clojure.pprint
        acyclic.utils.tinhole)
  (:require [clojure.core.typed :as t]
            [clojure.test :refer :all]))

(t/ann-record Point [x :- Number, y :- Number])
(t/ann-record Color [r :- Short, g :- Short, b :- Short])
(t/ann-record Turtle [position :- Point,
                      color :- Color
                      heading :- Number])
(defrecord Point [x y])
(defrecord Color [r g b])
(defrecord Turtle [position color heading])

(def bruce (->Turtle (->Point 1.0 2.0)
                     (->Color (short 255) (short 0) (short 0))
                     (/ Math/PI 4)))

(th-get-in bruce [:position :x [inc dec]])
(th-assoc-in bruce [:position :x [inc dec]] 3)


(t/ann ^:no-check Cos (t/IFn [Number -> Number]))
(t/ann ^:no-check Sin (t/IFn [Number -> Number]))
(defn Cos [a] (Math/cos a))
(defn Sin [a] (Math/sin a))
(t/defn movexy [x :- Number
                y :- Number
                dir :- Number
                dist :- Number] :- (t/HVec [Number Number])
                [(+ x (* dist (Cos dir)))
                 (+ y (* dist (Sin dir)))])

(t/ann mover (t/IFn [Turtle Number -> Turtle]))
(def mover (mk-th-mod movexy 2 [:position :x] [:position :y] [:heading]))

(def path-dict {:x [:position :x]})
(th-assoc path-dict bruce :x 5)
(th-assoc path-dict bruce :x 5 :heading Math/PI)

(t/defalias Silly "silly map" (t/HMap :mandatory {:a (t/HMap :mandatory {:b t/Str})}))
(t/defalias Billy "stuff in b" (t/HMap :mandatory {:c t/Num}))

(t/ann ^:no-check s->billy (t/IFn [t/Str -> Billy]))
(t/ann ^:no-check billy->s (t/IFn [Billy -> t/Str]))
(defn s->billy [s] (read-string s))
(defn billy->s [b] (pr-str b))

(t/ann x Silly)
(def x {:a { :b "{:c 3}"}})

(t/ann g (t/IFn [Silly -> t/Num]))
(def g (mk-th-get [:a :b [billy->s s->billy] :c]))

(def y (g x))





