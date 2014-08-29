(ns acyclic.utils.pinhole)

;; Pinhole utilities.  Like lenses but simpler.

(defn- ph-assoc1 [o paths [k v]]
  (let [ks (if (sequential? k) k (paths k))
        ks (or ks [k])
        f  (first ks)
        [ks v] if ((fn? f) [(rest ks) (f v)] [ks v])]
    (assoc-in o ks v)))

(defn ph-assoc [o paths & kvs]
  "Pinhole association.  o comprises nested maps or records, paths is a map of entries
[synonym assoc-in-style-vec].  Subsequent key value pairs are associated into o normally
unless the key is found in the path map, in which case it's assoc-in'd appropriately.
Returns the \"modified\" o."
  (reduce #(ph-assoc1 %1 paths %2) o (partition 2 kvs)))

(defn ph-get [o paths k]
  (let [ks (if (sequential? k) k (paths k))
        ks (or ks [k])]
    (get-in o ks)))

(defn mk-pinhole-set [paths k]
  (fn [o v] (ph-assoc o paths k v)))

(defn mk-pinhole-get [paths k]
  (fn [o] (ph-get o paths k)))


(comment

  (defrecord Boffo [foo bar])
  (defrecord Canard [boffo])
  (def paths {:bark [:boffo :bar]})

  (def c (->Canard (->Boffo "hi" "bye")))

  (def set-bar (mk-pinhole-set paths :bark))
  (def get-bar (mk-pinhole-get paths :bark))
  

)
