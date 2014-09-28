(ns acyclic.utils.tinhole
  (:use clojure.walk clojure.pprint)
  (:require [clojure.core.typed :as t]
            [acyclic.utils.pinhole :as ph]))

(defmacro th-get-in [m path]
  (reduce (fn [acc k] (concat acc (list (if (vector? k)
                                          `(~(second k))
                                          `(get ~k)))))
          `(-> ~m) path))


(t/ann th-assoc-in-gen (t/IFn [t/Any (t/NonEmptySeq t/Any) t/Any -> t/Any]))
(defn- th-assoc-in-gen [m ks v]
  (let [k  (first ks)
        ks (next ks)]
    (cond
     (vector? k) (let [[f-in f-out] k]
                   (list f-in
                         (if-not ks v (th-assoc-in-gen (list f-out m) ks v))))
     ks        (list 'assoc m k (th-assoc-in-gen (list 'get m k) ks v ))
     :else     (list 'assoc m k v))))

(defmacro th-assoc-in [m ks v]
  (th-assoc-in-gen m ks v))

(defmacro th-assoc
  [path-dict m & kvs]
  (let [path-dict (eval path-dict)]
    (reduce (fn [acc [k v]]
              (concat acc (list `(th-assoc-in ~(ph/condition-key path-dict k) ~v))))
            `(-> ~m)
            (partition 2 kvs))))


(defmacro th-get 
  [path-dict m k]
  `(th-get-in ~m ~(ph/condition-key (eval path-dict) k)))

(defmacro mk-th-set
  ([ks] `(fn [o# v#] (th-assoc-in o# ~ks v#))))

(defmacro mk-th-get [ks]  `(fn [o#] (th-get-in o# ~ks)))

;(t/ann gensyms (t/IFn [t/Str t/Int -> (t/Seq t/Sym)]))
(t/defn gensyms [s :- t/Str n :- t/Int] :- (t/Seq t/Sym)
  (let [s  (gensym (str s "-"))]
    (map #(symbol (str s "-" %)) (range n))))

(defmacro mk-th-mod [f n-out n-more & arg-paths]
  (let [o       (symbol (name (gensym "obj-")))
        n-args  (count arg-paths)
        args    (gensyms "arg" n-args)
        margs   (gensyms "more-arg" n-more)
        argvals (map #(list 'th-get-in o %) arg-paths)
        fnvals  (gensyms "fv" n-out)]
    `(fn [~o ~@margs] 
       (let [~@(interleave args argvals)
             [~@fnvals] (~f ~@args ~@margs)]
         (-> ~o 
             ~@(map #(list 'th-assoc-in (nth arg-paths %1) (nth fnvals %1)) (range n-out) ))))))



