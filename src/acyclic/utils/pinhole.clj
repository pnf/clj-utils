(ns acyclic.utils.pinhole)

;; Pinhole utilities.  Like lenses but more primitive technology.

(defn- ph-assoc1 [o paths [k v]]
  (let [ks (if (sequential? k) k (paths k))
        ks (or ks [k])
        f  (first ks)
        [ks v] (if (fn? f) [(rest ks) (f v)] [ks v])]
    (assoc-in o ks v)))

(defn ph-assoc [o paths & kvs]
  "Pinhole association.  o comprises nested maps or records, paths is a map like
    {:key1 :new-key  ;or
     :key2 [:path :just-like :assoc-in] ...}
   and kvs are alternating keys and values, as in assoc.
If a key is not found in paths, it is assoc'd normally.
If a key is found in paths, associated with a scalar, then the value will be assoc'd in with that scalar as a key.
If a key is found in paths, associated with a sequence, then the value will be assoc-in'd with that sequence,
except that if the first item in the sequence, the result of its application to the value will be assoc'd in
with the rest of the sequence.
Returns the \"modified\" o."
  (reduce #(ph-assoc1 %1 paths %2) o (partition 2 kvs)))

(defn ph-get [o paths k]
"Pinhole lookup.  o comprises nested maps or records, paths is a map like
    {:key1 :new-key  ;or
     :key2 [:path :just-like :assoc-in] ...}
   and k is a key.
If a key is not found in paths, it is retrieved normally with get.
If a key is found in paths, associated with a scalar, then the value in o keyed by that scalar will be retrieved.
If a key is found in paths, associated with a sequence, then that sequence will be used to retrieve the value with get-in,
Returns the value."
  (let [ks (if (sequential? k) k (paths k))
        ks (or ks [k])]
    (get-in o ks)))

(defn mk-pinhole-assoc [paths k]
  "Returns a function that can be used like assoc, but internally uses the path as in ph-assoc."
  (fn [o v] (ph-assoc o paths k v)))

(defn mk-pinhole-get [paths k]
  "Returns a function that can be used like get, but internally uses the path as in ph-get"
  (fn [o] (ph-get o paths k)))

(defn- get+
  "Acts like get if ks is not sequential.  Otherwise acts like get-in, except that
any IFn arguments are applied in lieu of the next get."
  [m ks]
  (if-not (sequential? ks)
    (get m ks)
    (loop [m        m
           [k & ks] ks]
      (if-not k m (recur (if (ifn? k) (k m) (m k)) ks)))))

(defn x-entry
  [m k]
  (if-not (sequential? k)
    [k (get m k)]
    [(first  k) (get+ m (second k))]))

(defn mseq->m
  "Return a map containing, for each map m in sequence mseq, {(m k) m}"
 [mseq k]
  (into {} (map  #(vector (% k) %) mseq)))

(defn m-section
  "Cross section of a map.  Given a sequence of key-specifiers ks, returns a map containing only the specified keys.
   A key-specifier can take the forms
      :some-key
      [:new-key-name :old-key-name]
      [:new-key-name [:path :to :entry :ala :get-in]
   Additionally, if any item in the path is a function, then it will be applied to the
structure gotten so far, rather than used as a key.  Typically, a function would appear
as the last entry and be used to transform the result."
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


(comment

  (defrecord Boffo [foo bar])
  (defrecord Canard [boffo])
  (def paths {:bark [:boffo :bar]})

  (def c (->Canard (->Boffo "hi" "bye")))

  (def set-bar (mk-pinhole-set paths :bark))
  (def get-bar (mk-pinhole-get paths :bark))

  (m-section {:a {:b 4} :d 5 :e 6} [:d  [:foo [:a :b inc]]])
  ;; ==> {:d 5, :foo 5}
  

)
