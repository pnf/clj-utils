(ns acyclic.utils.pinhole)

;; Pinhole utilities.  Like lenses but more primitive technology.

(defn ph-get-in
  "Returns the value in a nested associative structure,
  where ks is a sequence of keys. Returns nil if the key
  is not present, or the not-found value if supplied.
  If any element is a vector, the second element of that
  vector is applied as a function to the structure before
  proceeding; should the vector element be at the end of
  the key sequence, the function is applied to the final
  result, e.g.
  (ph-get-in {:a {:b \"{:c 1}\"}} [:a :b [pr-str read-string] :c [dec inc]])"
  [m [k & ks]]
  ;;(println "   m=" (pr-str m) "k=" k)
  (cond
   (vector? k) (ph-get-in ((second k) m) ks)
   k           (ph-get-in (get m k) ks)
   :else       m))

(defn ph-assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created.
  If any key is a vector, the first element of the vector will be taken as a function to
  be applied to the next iteration." 
  [m [k & ks] v]
  ;;(println "   m=" (pr-str m) "k=" k)
  (cond
   (vector? k)  (let [[f-in f-out] k]
                  (f-in (if-not ks v (ph-assoc-in (f-out m) ks v))))
   ks          (assoc m k (ph-assoc-in (get m k) ks v))
   k           (assoc m k v)
   :else       v))


(defn- condition-key
  ([k]
     (if (sequential? k) k [k]))
  ([path-dict k]
     (cond
      (sequential? k) k
      (path-dict k)       (path-dict k)
      :else           [k])))

(defn ph-assoc
  "Pinhole association.  o comprises nested maps or records, path-dict is a map like
    {:key1 :new-key  ;or
     :key2 [:path :just-like :assoc-in] ...}
   and kvs are alternating keys and values, as in assoc.
If a key is not found in path-dict, it is assoc'd normally.
If a key is found in path-dict, associated with a scalar, then the value will be assoc'd in with that scalar as a key.
If a key is found in path-dict, associated with a sequence, then the value will be assoc-in'd with that sequence,
except that if the first item in the sequence, the result of its application to the value will be assoc'd in
with the rest of the sequence.
If the first element of the path is a vector, then the first element of that vector is assumed to be a function
that will be applied to the value before insertion.
Returns the \"modified\" o."
  [path-dict m & kvs]
  (reduce (fn [m [k v]]
            (ph-assoc-in m (condition-key path-dict k) v)) m (partition 2  kvs)))


(defn ph-get
"Pinhole lookup.  o comprises nested maps or records, path-dict is a map like
    {:key1 :new-key  ;or
     :key2 [:path :just-like :assoc-in] ...}
   and k is a key.
If a key is not found in path-dict, it is retrieved normally with get.

If a key is found in path-dict, associated with a sequence, then that sequence will be used to retrieve the value with get-in,
Returns the value.
If the first element of a path is a vector, then the second element of that vector is assumed to be a function
applied to the final result."
 [path-dict o k]
 (ph-get-in o (condition-key path-dict k)))

(defn mk-ph-set
  "Returns a function that can be used like assoc, but internally uses the path as in ph-assoc."
  ([path-dict k] (mk-ph-set (condition-key path-dict k)))
  ([ks]
     (fn [o v] (ph-assoc-in o ks v))))

(defn mk-ph-get
  "Returns a function that can be used like get, but internally uses the path as in ph-get"
  ([path-dict k] (mk-ph-get (condition-key path-dict k)))
  ([ks] (fn [o] (ph-get-in o ks))))

(defn mk-ph-apply
  "Returns a function that modifies within the structure."
    [ks]
    (fn [o f] (ph-assoc-in o ks  (f (ph-get-in o ks)))))

(defn- condition-function [path-dict f]
  (if-not
      (sequential? f) [f]
      (let  [[f & ks] f]
        [f (map (partial condition-key path-dict) ks)])))

(defn mk-ph-mod [f & arg-paths]
  (if (map? (first arg-paths))
    (let [[path-dict & arg-paths] arg-paths
          arg-paths  (map (partial condition-key path-dict) arg-paths )]
      (apply mk-ph-mod f arg-paths))
    (fn [o & more-args]
      (let [args (map (partial ph-get-in o) arg-paths)
            vs   (apply f (concat args more-args))
            kvs  (map vector arg-paths vs)]
        (reduce (fn [m [k v]] (ph-assoc-in m k v)) o kvs)))))

;;;;;;;;;;;;;;;;

(defn- x-entry
  [m k]
  (if-not (sequential? k)
    [k (get m k)]
    [(first k) (ph-get-in m (condition-key  (second k)))]))


(defn m-section
  "Cross section of a map.  Given a sequence of key-specifiers ks, returns a map containing only the specified keys.   A key-specifier can take the forms
      :some-key
      [:new-key-name :old-key-name]
      [:new-key-name [:path :to :entry :ala :get-in]
See also ph-get-in."
  [m ks]
  (into {} (map (partial x-entry m) ks)))

(defn extract-keys-from-map
    "Given opts of form {:k1 v1 :k2 v2 ..}, return [opts' vs] where
vs are values corresponding to ks and opts' is opts with those pairs removed."
    [opts ks]
    (let [vals (map opts ks)
          opts (apply dissoc opts ks)]
      [opts vals]))

(defn extract-opts
  "Given opts of form [:k1 v1 :k2 v2 ..], return [opts' vs] where
vs are values corresponding to ks and opts' is opts with those pairs removed."
  [opts ks]
  (let [opts        (apply hash-map opts)
        [opts vals] (extract-keys-from-map opts ks)
        opts        (flatten (seq opts))]
    [opts vals]))

;; scala> case class Point(x: Double, y: Double)
;; defined class Point

;; scala> case class Color(r: Byte, g: Byte, b: Byte)
;; defined class Color

;; scala> case class Turtle(
;;          position: Point,
;;          heading: Double,
;;          color: Color)

;; scala> Turtle(Point(2.0, 3.0), 0.0,
;;          Color(255.toByte, 255.toByte, 255.toByte))
;; res0: Turtle = Turtle(Point(2.0,3.0),0.0,Color(-1,-1,-1))


(comment

)
