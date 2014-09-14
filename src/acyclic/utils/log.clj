(ns acyclic.utils.log
  (:require
   [taoensso.timbre :as timbre]
   [taoensso.carmine :as car :refer (wcar)]
   [taoensso.timbre.appenders.carmine :as car-appender]))
(timbre/refer-timbre)

(defn stack-trace [e]
  (let [sw (java.io.StringWriter.)
        pw (java.io.PrintWriter. sw)
        _  (.printStackTrace e pw)
        s  (.toString sw)
        ls (clojure.string/split-lines s)]
    (vec (map #(clojure.string/replace % "\tat " "") ls))))

(def OPTPAT
  (re-pattern (str "\\b" (clojure.string/join "|" (vals clojure.lang.Compiler/CHAR_MAP)) "\\b" )))

(def  STR->OPT
  (apply hash-map (mapcat #(vector (second %) (str (first %))) clojure.lang.Compiler/CHAR_MAP)))

(defn fname
  "Extract the qualified name of a clojure function as a string."
  [f]
  (-> (str f)
      (clojure.string/replace OPTPAT STR->OPT)
      (clojure.string/replace-first "$" "/")
      (clojure.string/replace #"@\w+$" "")
      (clojure.string/replace #"_" "-")))

(def log-atom (atom []))
(def redis-atom (atom nil))
(defn set-logging! [log]

  (when log 
    (let [[level host port] (clojure.string/split log #"\:")
          _ (println level host port)
          level             (keyword level)
          port              (and port (Integer/parseInt port))]
      (if port
        (do
          (reset! redis-atom {:pool {} :spec {:host host :port port}})
          (wcar @redis-atom car/ping)
          (timbre/set-config!
           [:appenders :carmine]
           (try 
             (car-appender/make-carmine-appender {}  {:conn-opts @redis-atom})
             (catch Exception e (throw (ex-info
                                        "Carmine appender error"
                                        {:data (ex-data e)
                                         :msg (.getMessage e)
                                         :orig (.getCause e)
                                         :orig-msg (-> e .getCause .getMessage) }))))))
        (timbre/set-config!
         [:appenders]
         {:accrue
          {:min-level nil :enabled? true :async? false :rate-limit nil
           :fn (fn [log-entry]
                 (swap! log-atom (fn [log-vec] conj log-vec log-entry)))}
          :stderr
          {:min-level nil :enabled? true :async? false :rate-limit nil
           :fn (fn [{:keys [error? output]}] ; Can use any appender logs
                 (binding [*out* *err*]
                   (timbre/str-println output)))}}))
      (timbre/set-level! (or level :info)))))


(def iida (atom 0))
(defn iid [& s]
  (if (timbre/level-sufficient? :trace nil)
    (str  (clojure.string/join "-" (map str (filter #(not (nil? %)) s)) ) "-" (str (swap! iida inc)))
    nil))


(defn with-accrued-log [m] (assoc m :log @log-atom))
