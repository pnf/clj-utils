(ns acyclic.utils.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :as async :refer [<! >! <!! >!! timeout chan alt!! go close!]]
            [acyclic.utils.log :as ul]
            [taoensso.timbre :as timbre])
  (:gen-class))
(timbre/refer-timbre)

(def ^:dynamic *repl* false)

(defn- augment-cli-options [cli-options]
  (let [opts                (set (map #(->> % second (re-find #"--(\w+)\b") second keyword)
                                      cli-options))
        our                 (set (filter #(not (opts %)) [:help :hang :log :id]))]
    [our 
     (cond-> cli-options
             (our :help)
             (conj ["-h" "--help" "Nearly useless help."])
             (our :hang)
             (conj [nil "--hang SECS" "After launching, hang around for this period before exiting automatically."
                    :parse-fn #(Integer/parseInt %) :default 0])
             (our :log)
             (conj [nil "--log LEVEL" "Debug level" :parse-fn keyword :default nil])
             (our :id)
             (conj [nil "--id ID" "Any string identifier"]))]))

(defn- really-process-opts [args cli-options]
  (let [[our cli-options] (augment-cli-options cli-options)
         parsed            (parse-opts args cli-options)
         errs              (:errors parsed)
         opts              (:options parsed)
         opts              (merge (dissoc opts :opts) (:opts opts))]
    [our errs opts]))

(defn- wrapped-result [f opts log]
  (ul/set-logging! log)
  (let [t1   (.getTime (java.util.Date.))
        res  (try {:result (f opts)}
                  (catch Exception e {:exception (ul/stack-trace e)}))
        dt   (-  (.getTime (java.util.Date.)) t1)
        res  (merge res {:time dt :id (or (:id opts) "X")})
        res  (cond-> res log ul/with-accrued-log)]
    res))

(defn edn-app
  ([args cli-options f]  (edn-app false args cli-options f))
  ([repl args cli-options f]
     (let [[our errs opts] (really-process-opts args cli-options)]
       (cond errs
             (do 
               (println cli-options opts errs)
               (when-not repl (System/exit 1))
               nil)
             (and (our :help) (:help opts))
             (do 
               (println cli-options)
               (when-not repl (System/exit 0))
               nil)
             :else
             (let [res  (wrapped-result f opts (and (our :log) (:log opts)))]
               (if repl
                 (pr-str res)
                 (do 
                   (println (pr-str res))
                   (when (and (our :hang) (:hang opts))
                     (Thread/sleep (* 1000 (:hang opts))))
                   (System/exit 0))))))))
(defn edn-app-repl [args cli-options f] (edn-app true args cli-options f))

;;;  Everything below is for testing

(defn- bogus-opts [] [[nil "--bogus VAL" :parse-fn #(Integer/parseInt %)]])

(defn- bogus [opts]
  (debug "Here is a log entry")
  (str "The answer is " (float (/ 1 (:bogus opts)))))

(defn -main [& args]
  (edn-app args (bogus-opts) bogus))

