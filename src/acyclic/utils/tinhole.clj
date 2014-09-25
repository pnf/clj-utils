(ns acyclic.utils.tinhole
  (:use clojure.walk clojure.pprint)
  (:require [clojure.core.typed :as t]))

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

(defmacro gettin [m path]
  (reduce (fn [m k] (concat m (list (if (vector? k)
                                      `(~(second k))
                                      `(get ~k)))))
          `(-> ~m) path))


(comment

(t/ann putin (t/Fn [t/Map
                    (t/Vec (t/U t/Sym 
                                (t/HVec [(t/Fn [t/Any -> t/Map])
                                         (t/Fn [t/Map -> t/Any])])))
                     t/Any
                     -> t/Map]))
(defn putin [m [k & ks] v]
  (cond
   (vector? k) (let [[f-in f-out] k]
                 (list f-in
                       (if-not ks v (putin (list f-out m) ks v))))
   ks          (list 'assoc m k (putin (list 'get m k) ks v ))
   k           (list 'assoc m k v)
   :else       v))


(defmacro puttin [m ks v]
  (putin m ks v))

)

(t/ann putin (t/IFn [t/Any (t/NonEmptySeq t/Sym) t/Any -> t/Any]))
(defn putin [m ks v]
  (let [k  (first ks)
        ks (next ks)]
    (cond
     (vector? k) (let [[f-in f-out] k]
                   (list f-in
                         (if-not ks v (putin (list f-out m) ks v))))
     ks        (list 'assoc m k (putin (list 'get m k) ks v ))
     :else     (list 'assoc m k v))))

(defmacro puttin [m ks v]
  (putin m ks v))

(defmacro mk-th-set [ks] `(fn [o# v#] (puttin o# ~ks v#)))

(defmacro mk-th-get [ks]
  `(fn [o#] (gettin o# ~ks)))

(t/ann gensyms (t/IFn [t/Str t/Int -> (t/Seq t/Sym)]))
(defn gensyms [s n]
  (let [s  (gensym s)]
    (map #(symbol (str s "-" %)) (range n))))

(defmacro mk-th-mod [f n-out & arg-paths]
  (let [o       (symbol (name (gensym "o")))
        n-args  (count arg-paths)
        args    (gensyms "arg" n-args)
        n-more  (- (count args) n-out)
        margs   (gensyms "marg" n-more)
        argvals (map #(list 'gettin o %) arg-paths)
        fnvals  (gensyms "fv" n-out)]
    `(fn [~o ~@margs] 
       (let [~@(interleave args argvals)
             [~@fnvals] (~f ~@args ~@margs)]
         (-> ~o 
             ~@(map #(list 'puttin (nth arg-paths %1) (nth fnvals %1)) (range n-out) ))))))

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


(t/defalias Silly "silly map" (t/HMap :mandatory {:a (t/HMap :mandatory {:b t/Str})}))
(t/defalias Billy "stuff in b" (t/HMap :mandatory {:c t/Num}))

(t/ann ^:no-check s->billy (t/IFn [t/Str -> Billy]))
(t/ann ^:no-check billy->s (t/IFn [Billy -> t/Str]))
(def s->billy read-string)
(def billy->s pr-str)


(t/ann x Silly)
(def x {:a { :b "{:c 3}"}})

(t/ann g (t/IFn [Silly -> t/Num]))
(def g (mk-th-get [:a :b [billy->s s->billy] :c]))

(def y (g x))

(comment





)
