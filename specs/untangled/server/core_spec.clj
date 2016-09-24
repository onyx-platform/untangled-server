(ns untangled.server.core-spec
  (:require [untangled.server.core :as core]
            [untangled-spec.core :refer [specification behavior provided component assertions]]
            [untangled.server.impl.pretty-system :as pretty-system]
            [com.stuartsierra.component :as component]))

(specification "transitive join"
  (behavior "Creates a map a->c from a->b combined with b->c"
    (assertions
      (core/transitive-join {:a :b} {:b :c}) => {:a :c})))

(specification "make-untangled-server"
  (assertions
    "requires :parser as a parameter, and that parser be a function"
    (core/make-untangled-server) =throws=> (AssertionError #"")
    (core/make-untangled-server :parser 'cymbal) =throws=> (AssertionError #"")
    "requires that :components be a map"
    (core/make-untangled-server :parser #() :components [1 2 3]) =throws=> (AssertionError #"")
    "throws an exception if injections are not keywords"
    (core/make-untangled-server :parser #() :parser-injections [:a :x 'sym]) =throws=> (AssertionError #""))
  (let [stack (volatile! [])]
    (with-redefs [pretty-system/log
                  (constantly nil)
                  pretty-system/print-throwable
                  (constantly nil)
                  pretty-system/pr-pretty-str
                  (fn [x] (vswap! stack conj x))]
      (component/start
        (core/make-untangled-server
          :parser #(), :config-path "/fubar"
          :pretty/big-data #{:api-parser :config-path})))
    (assertions
      "we can pretty print the system when it errors"
      ;;BIG DATA IS PRINTED FIRST
      (keys (get-in @stack [0])) => [:config-path]
      (keys (get-in @stack [1])) => [:api-parser]
      ;;THE SYSTEM IS PRINTED WITH BIG DATA REPLACED WITH A LINK
      (get-in (first (take-last 2 @stack))
              [1 :handler :api-parser])
      => :system/big-data
      ;;THE COMPONENT IS PRINTED LAST
      (get-in (last @stack) [1])
      => {:value nil :config-path "/fubar"})))
