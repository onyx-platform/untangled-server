(ns untangled.server.impl.pretty-system
  (:require
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]]
    [com.stuartsierra.component :as component]
    [io.aviso.exception :as pretty]
    [taoensso.timbre :as timbre]))

(defn- clean [sys big-data]
  (let [top-level-key? (set (keys sys))]
    [(type sys)
     (into {}
       (map (fn [[k v]]
              [k (walk/prewalk
                   (fn [x]
                     (if (map-entry? x)
                       (cond (top-level-key? (key x))
                             [(key x) :system/top-level]
                             (big-data (key x))
                             [(key x) :system/big-data]
                             :else x)
                       x))
                   v)]))
       sys)]))

(defn print-throwable [e & [limit]]
  (print (pretty/format-exception e {:frame-limit (or limit 10)}))
  (some-> (.getCause e) (print-throwable limit)))

(defn log [level & args]
  ((case level
     :trace #(timbre/trace %)
     :debug #(timbre/debug %)
     :error #(timbre/error %))
   (apply str (interpose " " args))))

(defn pr-pretty-str [x]
  (with-out-str (pprint x)))

(defn wrap-system [system & {:keys [pretty/big-data]}]
  (let [big-data (or big-data #{})]
    (reify
      component/Lifecycle
      (start [_]
        (try (component/start system)
          (catch Throwable t
            (if-let [{:keys [system component]}
                     (and (component/ex-component? t)
                       (ex-data t))]
              (do
                (when big-data
                  (log :debug "To see :pretty/big-data make sure the timbre log level is trace"))
                (doseq [cmp (filter (if-not (seq big-data) (constantly false)
                                      (apply some-fn big-data))
                                    (vals system))]
                  (log :trace (str cmp " - " big-data ":\n")
                       (pr-pretty-str (select-keys cmp big-data))))
                (log :debug "System:\n" (pr-pretty-str (clean system big-data)))
                (log :error "Failing component:\n" (pr-pretty-str (clean component big-data)))
                (print-throwable (component/ex-without-components t)))
              (print-throwable t)))))
      (stop [_]
        (component/stop system)))))
