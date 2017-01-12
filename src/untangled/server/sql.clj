(ns untangled.server.sql
  "Support functions for dealing with SQL results in the context of Om responses"
  (:require [clojure.walk :as walk]
            [om.next :as om]))

(defn- decode-row [codecs row]
  (reduce (fn [row [k v]]
            (if-let [codec (get-in codecs [k :in])]
              (assoc row k (codec v))
              row)) row row))

(defn decode-rows [rows codecs]
  (map (partial decode-row codecs) rows))

(defn- build-ident [kw row]
  [kw (get row kw)])

(defn- get-entity [ns row]
  (let [ns (name ns)]
    (reduce (fn [entity [k v]]
              (if (= ns (namespace k))
                (assoc entity k v)
                entity)) {} row)))

(defn- derive-ident
  "Looks at a given map with possible ident IDs. Returns the first ident it can find
  that has a non-nil ID.

  Returns [ident entity-with-db-id], or nil if none are found."
  [entity possible-ns]
  (let [possible-ids (map #(keyword (name %) "id") possible-ns)
        all-idents (map (fn [kw] (build-ident kw entity)) possible-ids)
        valid-idents (keep (fn [[k id]] (when-not (nil? id)
                                          [k id])) all-idents)
        ident (first valid-idents)
        id-key (first ident)
        id (second ident)
        entity (when ident
                 (-> entity (dissoc id-key)
                     (assoc :db/id id)))]
    (when entity
      [ident entity])))

(defn- entities-in-row
  "Find all entities in a row, and return this as a sequence of pairs: [ident entity].

  possible-entities can be a keyword or string to represent the namespace."
  [row possible-entities]
  (let [entities (map #(get-entity % row) possible-entities)
        result (keep #(derive-ident % possible-entities) entities)]
    result))

(defn get-all-entities
  [decoded-rows possible-entities]
  (reduce (fn [entities row]
            (into entities (entities-in-row row possible-entities)))
          #{} decoded-rows))

(defn get-om-result
  [all-entities]
  (reduce (fn [graph-db [ident entity]] (assoc-in graph-db ident entity))
          {} all-entities))

(defn postwalk-update-values-remove-nulls
  [kword kword-value form]
  (clojure.walk/postwalk (fn [x] (cond
                                   (and (vector? x) (= nil (last x))) nil
                                   (and (vector? x) (= kword (first x))) [kword (keyword (str kword-value (last x)))]
                                   :else x
                                   )) form))

(defn postwalk-prepare-dates
  [kword form]
  (clojure.walk/postwalk (fn [x] (if (and (vector? x) (= kword (first x)))
                                   [kword ((symbol "#inst") (second x))]
                                   x) ) form))

(defn replace-keywords-and-nulls-for-map [m col]
  (reduce #(postwalk-update-values-remove-nulls (first %2) (second %2) %1) m col))


(defn- decode-graph-edge
  "Add a graph edge to the given graph-db (which must already have the entities in it).

  row is a row of the result set
  from is the property on the entity that should point towards another entity
  to is the property on the result row that contains the ID of the target
  arity is :many or :one

  Returns a new graph-db with the edge in place.
  "
  [graph-db row from to arity]
  (let [from-type (namespace from)
        from-id-kw (keyword from-type "id")
        from-id (get row from-id-kw)
        from-ident [from-id-kw from-id]
        to-id (get row to)
        ;if single arity and doesn't have an id (i.e. no ident) use a map else vector
        to-ident (if (and (= :one arity) (not= "id" (name to))) {to to-id}[to to-id])
        valid-idents? (and to-id from-id)]
    (if valid-idents?
      (if (= :many arity)
        ; FIXME: ? HIGHLY INEFFICIENT (comp vec distinct conj)
        (update-in graph-db from-ident update from (fnil (comp vec distinct conj) []) to-ident)
        (update-in graph-db from-ident assoc from to-ident))
      graph-db)))

(defn- decode-graph-edges-on-row
  [graph-db row graph-edges]
  (reduce (fn [db edge] (apply decode-graph-edge db row edge)) graph-db graph-edges))

(defn decode-graph-edges
  [graph-db rows graph-edges]
  (reduce (fn [db row] (decode-graph-edges-on-row db row graph-edges)) graph-db rows))

(defn- all-idents
  "Returns all of the idents for a given table (e.g. :person/id)"
  [graph-db table]
  (let [ids (keys (get graph-db table))]
    (mapv #(vector table %) ids)))

(defn add-key-to-graph
  "Places all of the idents for a given om-style table under a given key."
  [graph api-key id-key]
  (assoc graph api-key
               (all-idents graph id-key)))

(defn replace-temp-keys
  "Replace keys in `graph` with values from `replacements`."
  [replacements graph]
  (walk/postwalk-replace replacements graph))

;; END Table->Tree helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO:
;; - Add a dev card example.
;; - Make potentially useful private helpers public.
;; - Write more tests!
(defn table->tree
  "Transform flat SQL query results into graph data required by the client.

  `pg-result` A flat hash map of SQL query result, with keys namespaced by
              entity.
  `om-query`  The Om Next query of the client component that made the request.
  `rules`     A map of info required to transform the data, containing the
              following keys:

              `:refs`     A vector of `get-in` style paths, pointing to
                          sub-entitles to be nested. The last element of each
                          path must be either the keyword `:one` or `:many`.

              `:replace`  A map of keywords in `pg-result`, with values to use
                          as replacements.

              `:codecs`   Optional. A map keyed by attribute keywords, with
                          values of the form
                          `{:in transform-fn-A :out transform-fn-B}`. The
                          `:out` function will be used to transform the value
                          of the attribute at the given keyword.

              `:entities` A set of keywords naming the possible entities
                          nested in the resulting graph.

              `:api-key`  The resulting data will be nested under this key in
                          the result.

              `:id-key`   A keyword indicating the the top-level table id (each
                          entity id in `sql-result` should have a unique keyword,
                          i.e. `:person/id`, `:phone/id`, etc.).

  *Example*
  ```
  (let [sql-result '({:person/id    1
                      :person/name  \"Joe\"
                      :phone/id     1
                      :phone/type   \"home\"
                      :phone/number \"555-1212\"
                      :billing/id   1
                      :billing/cc   \"3333-4444-5555-1111\"})

        om-query [{:people [:db/id :person/name
                            {:person/phone [:db/id :number :type]}
                            {:person/billing [:db/id :billing/cc]}]}]

        rules {:refs     [[:person/phone :phone/id :many]
                          [:person/billing :billing/id :one]]
               :replace  {:phone/number :number
                          :phone/type   :type}
               :codecs   {:phone/type {:in  keyword
                                       :out name}}
               :entities #{:person :phone :billing}
               :id-key   :person/id}]

    (api/table->tree sql-result om-query rules))

  ;=> {:people [{:db/id 1
  ;=>            :person/name    \"Joe\"
  ;=>            :person/phone   [{:db/id        1
  ;=>                              :number \"555-1212\"
  ;=>                              :type   :home}]
  ;=>            :person/billing {:db/id      1
  ;=>                             :billing/cc \"3333-4444-5555-1111\"}}]}
  ```
  "
  [sql-result om-query rules]
  (let [{:keys [refs replace codecs entities id-key]} rules
        decoded-rows (decode-rows sql-result codecs)
        all-entities (get-all-entities decoded-rows entities)
        om-result (get-om-result all-entities)
        normalized-graph (decode-graph-edges om-result decoded-rows refs)
        db-key :temp-key
        keyed-graph (add-key-to-graph normalized-graph db-key id-key)
        graph (replace-temp-keys replace keyed-graph)
        denormalized-graph (om/db->tree [{db-key om-query}] graph graph)]
    (get denormalized-graph db-key)))
