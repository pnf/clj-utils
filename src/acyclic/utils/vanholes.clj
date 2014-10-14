(ns acyclic.utils.vanholes
  (:use clojure.algo.monads)
  (:require [clojure.core.typed :as t]
            [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer [pprint]]))





(t/ann-protocol [[a :variance :covariant]] IFunctor
                p-fmap
                (t/All [b] (t/IFn [(IFunctor a) [a -> b] -> (IFunctor b)])))

(t/defprotocol  ;; [[a :variance :covariant]]
  IFunctor
  (p-fmap [this f]))

(t/ann-record [[a :variance :covariant]] Identity [runIdentity :- a])
(defrecord Identity [runIdentity])
;; (t/ann identity-fmap (t/All [a b] (t/IFn [(Identity a) [a -> b] -> (Identity b)])))
;; Type Error (acyclic/utils/boffo.clj:25:1) Type mismatch:
;; Expected: 	(clojure.core.typed/HMap :optional {:p-fmap [(acyclic.utils.boffo.Identity clojure.core.typed/Any) clojure.core.typed/Any -> clojure.core.typed/Any]})
;; Actual: 	(clojure.core.typed/HMap :mandatory {:p-fmap (clojure.core.typed/All [a b] [(acyclic.utils.boffo.Identity a) [a -> b] -> (acyclic.utils.boffo.Identity b)])} :complete? true)
(t/ann ^:no-check identity-fmap (t/All [a] (t/IFn [(Identity a) t/Any -> t/Any])))
(defn identity-fmap [this f] (->Identity (f (:runIdentity this))))

(extend Identity
  IFunctor
  {:p-fmap identity-fmap}  )

(t/ann-record [[a :variance :covariant]] Const [getConst :- a])
(defrecord Const [getConst]
  IFunctor
  (p-fmap [this f] this))


(t/defalias Functor (t/TFn [[a :variance :covariant]] (Extends [(IFunctor a)]) ))
(t/defalias DumbFunctor (t/TFn [[a :variance :covariant]] (t/U (Identity a) (Const a))))

#_(t/ann ^:no-check fmap
       (t/All [a b] (t/IFn [[a -> b] (Functor a) -> (Functor b)]) ))


(t/ann ^:no-check fmap
       (t/All [a b] (t/IFn [[a -> b] (DumbFunctor a) -> (DumbFunctor b)]) ))

#_(t/ann ^:no-check  fmap
       (t/All [[f :< Functor] a b]
              (t/IFn [[a -> b] (f a) -> (f b)]) ))

#_(t/ann ^:no-check  fmap
       (t/All [[f :< DumbFunctor] a b]
              (t/IFn [[a -> b] (f a) -> (f b)]) ))


(defn fmap [fun c] (p-fmap c fun))

;; type Lens s a = Functor f => (a -> fa) -> s -> f s
#_(t/defalias Lens (t/TFn [[s :variance :invariant]
                         [a :variance :invariant]
                         ]
                        [[a -> (Functor a)] s -> (Functor s)] ))

(t/defalias Lens (t/TFn [[s :variance :invariant]
                         [a :variance :invariant]]
                        [[a -> (DumbFunctor a)] s -> (DumbFunctor s)] ))

#_(t/defalias Lens (t/TFn [[s :variance :invariant]
                         [a :variance :invariant]]
                        (t/All [[f :< Functor]]
                               [[a -> (f a)] s -> (f s)]) ))

#_(t/defalias Lens (t/TFn [[s :variance :invariant]
                         [a :variance :invariant]]
                        (t/All [[f :< DumbFunctor]]
                               [[a -> (f a)] s -> (f s)]) ))

;; over  ::  Lens s a -> (a -> a) -> s -> s
;; over ln fs = runIdentity $ ln (Identity . f) s 
(t/ann ^:no-check over (t/All [a s] (t/IFn [(Lens s a) [a -> a] s -> s])))
(defn over [ln f s] (:runIdentity (ln #(->Identity (f %)) s)))

;; Set :: Lens s a -> a -> s -> s
;; set ln x = over ln (const x)
(t/ann ^:no-check lset (t/All [a s] [(Lens s a) a s -> s]))
(defn lset [ln x s] (over ln (constantly x) s))

;; view :: Lens s a -> s -> a
;; view ln s = getConst $ ln Const s
(t/ann ^:no-check view (t/All [a s] [(Lens s a) s -> a] ))
(defn view [ln s] (:getConst (ln ->Const s)))


(t/defalias I1 t/Int)
(t/defalias I2 t/Int)
(t/defalias I12 (t/HVec [I1 I2]))
(t/defalias FI (t/HMap :mandatory {:foo I12}))

;(t/ann ^:no-check isecond [(t/HVec [t/Int t/Int]) -> t/Int] )
;(t/ann ^:no-check isecond (t/All [a b] [(t/HVec [a b]) -> b]) )
(t/ann ^:no-check isecond [(t/HVec [t/Int t/Int]) -> t/Int])
(def isecond second)
;(t/ann ^:no-check ifirst  [(t/HVec [t/Int t/Int]) -> t/Int])
(t/ann ifirst ^:no-check [(t/HVec [t/Int t/Int]) -> t/Int])
(def ifirst first)



(t/ann l:foo (Lens (t/HMap :mandatory {:foo (t/HVec [t/Int t/Int])})
                   (t/HVec [t/Int t/Int])))
(defn l:foo [f m]
  (fmap
   (t/fn [x :- I12] :- FI
     (assoc m :foo x))
     (f (:foo m))))

(t/ann l-1 (Lens (t/HVec [t/Int t/Int]) t/Int))
(defn l-1 [fun xy]
  (fmap
   (t/fn [x :- t/Int] :-  (t/HVec [t/Int t/Int]) (vector x (isecond xy)))
   (fun (ifirst xy))))

(t/ann curry (t/All [a b c]
                    [[a b -> c] -> [a -> [b -> c]]]))
(defn curry [f] (fn [x] (fn [y] (f x y))))


(t/ann uncurry (t/All [a b c]
                      [[a -> [b -> c]] -> [a b -> c]]))
(defn uncurry [f] (fn [x y] ((f x) y)))



(t/ann lcomp (t/All [a b c d e]
                    [[[b -> c] d -> e]    ;;      [b -> c] -> [d -> e]
                     [a b -> c]           ;; a -> [b -> c]
                     ->
                     [a d -> e]]))        ;; [a             -> [d -> e]]
(defn lcomp [l1 l2] (uncurry (comp (curry l1) (curry l2))))
;(defn lcomp [& ls] (uncurry (apply comp (map curry ls))))

;; _1 :: Functor f => (a -> f a) -> (a,b) -> f (a,b)
;; _1 f (x,y) = fmap (\a -> (a, y)) (f x)
;;(t/ann l-1 [(t/IFn [t/Int -> (Functor t/Int)]) (t/HVec [t/Int t/Int]) -> (Functor (t/HVec [t/Int t/Int])) ])
;;(t/ann l-1 [(t/IFn [t/Int -> (DumbFunctor t/Int)]) (t/HVec [t/Int t/Int]) -> (DumbFunctor (t/HVec [t/Int t/Int])) ])
;;(defn l-1 [f [x y]] (fmap #(vector % y) (f x)))



;; {:foo [1 2]}


;;(t/ann l-1 (Lens (t/HVec [t/Int t/Int]) t/Int))
;;(t/ann l-1 (t/All [a b] (Lens (t/HVec [a b]) a) ))

(comment
  (t/ann l-1 (Lens I12 I1))
  (defn l-1 [f xy]
    (fmap
     (t/fn [x :- I1] :- I12 (vector x (isecond xy)))
     (f (ifirst xy)))))


(defmacro deflens [lname implant extract]
  `(defn ~lname [x->Fx# s#] (fmap (fn [x#] (~implant s# x#)) (x->Fx# (~extract s#)))))


(defmacro curry-n [n f]
  (let [args (repeatedly (dec n) #(gensym "arg"))]
    `(fn [x#] (fn [~@args] (~f x# ~@args)))))
(defmacro uncurry-n [n f]
  (let [args (repeatedly (dec n) #(gensym "arg"))]
    `(fn [x# ~@args] ((~f x#) ~@args))))

(defmacro comp-n [n & fs]
  `(uncurry-n ~n (fn [x#] (-> x#  ~@(map #(list (list 'curry-n n %)) (reverse fs))))))



(println (over l-1 inc [3 4]))
(println (lset l-1 9   [3 4]))
(println (view l-1 [3 4]))

(println (view l:foo {:foo [1 2]}))

(println (view (lcomp l:foo l-1) {:foo [1 2]}))

;;(println (view (lcomp l:foo l-1) {:bar [1 2]}))

