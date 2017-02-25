(ns  untangled.server.sql
  "Support functions for dealing with SQL results in the context of Om responses"
  (:require [clojure.walk :as walk]
            [om.next :as om]
            [clojure.string :as str]))

(defn- kw->sql
  "Convert a keyword to the name we use in SQL. Replaces - with _."
  [kw]
  (-> kw str (str/replace #"[-]" "_")))

(defn- decode-row
  [codecs row]
  (reduce (fn [row [k v]]
            (if-let [codec (get-in codecs [k :in])]
              (assoc row k (codec v))
              row)) row row))

(defn decode-rows
  [rows codecs]
  (map (partial decode-row codecs) rows))

(defn- build-ident
  [kw row]
  [kw (get row kw)])

(defn- get-entity
  [ns row]
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
                                   :else x))
                         form))

(defn postwalk-prepare-dates
  [kword form]
  (clojure.walk/postwalk (fn [x] (if (and (vector? x) (= kword (first x)))
                                   [kword ((symbol "#inst") (second x))]
                                   x)) form))


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
        to-ident (if (and (= :one arity) (not= "id" (name to))) {to to-id} [to to-id])
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

(defn- omprops->dbprops
  "Transform an Om query to use local db table/column names"
  [translations query]
  (walk/postwalk-replace translations query))

(defn- db-query
  "Convert an om query to an equivalent om format query, but with the properties renamed to match the SQL database table
  and column names"
  [schema om-query]
  (omprops->dbprops (get schema :omprop->dbprop {}) om-query))

(defn- query-attributes
  "Returns the top-level attributes of the given query (without db/id) as a vector."
  [query]
  (into [] (keep (fn [i] (cond
                           (map? i) (first (keys i))
                           (= :db/id i) nil
                           :else i)) query)))

(defn- table-for-query
  "Returns the table of the top-level properties of the given query as a string."
  [schema om-query]
  (let [dbquery (db-query schema om-query)
        keys (query-attributes dbquery)]
    (first (map namespace keys))))

(defn- column
  "Convert a property to an SQL column selector that has suffix in order to allow selecting from the same table more than once."
  [schema db-query-item table suffix]
  (cond
    (= :db/id db-query-item) (keyword (str table "." suffix) "id")
    (keyword? db-query-item) (if (= (namespace db-query-item) table)
                               (let [nspc (str table "." suffix)
                                     nm (name db-query-item)]
                                 (keyword nspc nm))
                               (throw (ex-info (str "Query property " db-query-item " is on the wrong table. Conversion failed") {})))
    :else (throw (ex-info "Columns must be represented as keywords" {}))))

(defn- next-suffix
  "Takes the current suffix. If nest? is false, increments the last component of the suffix. If nest? is true, adds
  a new sequence to the end of the suffix starting at 1.

  1 -no_nest-> 2 -nest-> 2.1"
  [current-suffix nest?]
  (if current-suffix
    (if nest?
      (str current-suffix ".1")
      (let [current-suffix (str current-suffix)
            components (str/split current-suffix #"[.]")
            last-component (last components)
            lc-n (inc (Integer/parseInt last-component))
            new-components (-> components
                               reverse
                               rest
                               (conj (str lc-n))
                               reverse)]
        (str/join "." new-components)))
    "1"))

(defn table-name
  "Takes a kw that is being used as an alias for a table/column (possibly suffixed for join clarification)
  and returns just the table name. e.g. :table.1.1/a => \"table\""
  [kw]
  (let [name (namespace kw)
        base-name (str/replace name #"[.].*$" "")]
    base-name))

(defn- columns*
  [schema db-query suffix]
  (assert (vector? db-query) "Query input must be a vector")
  (reduce (fn [{:keys [join-suffix cols] :as acc} item]
            (let [table (table-for-query schema db-query)]
              (cond
                (keyword? item) (assoc acc :cols (conj cols (column schema item table suffix)))
                (map? item) (let [subquery (first (vals item))]
                              {:cols        (into cols (:cols (columns* schema subquery join-suffix)))
                               :join-suffix (next-suffix join-suffix false)})
                :else (throw (ex-info "Unexpected query item" {:item item}))))) {:cols [] :join-suffix (next-suffix suffix true)} db-query))

(defn- columns
  "Get all of the properly suffixed SQL columns that must be queried in order to satisfy the given om-query"
  [schema om-query]
  (:cols (columns* schema (db-query schema om-query) "1")))

(defn- join
  "Give back a join specification for a specific join element from a query. from-suffix is the
  suffix of the table that the join is going FROM, and target-suffix is the current suffix to use on the TO table."
  [schema db-join-element from-suffix target-suffix]
  (assert (map? db-join-element) "Asked to generate join info for a non-map")
  (let [join-prop (first (keys db-join-element))
        schema-by-dbprop (into {} (map (fn [item] [(:db-prop item) item]) (:joins schema)))
        join-spec (or (get schema-by-dbprop join-prop) (throw (ex-info "Om join has no join config" {:join join-prop})))
        join-spec-instance (assoc join-spec :from-alias from-suffix :to-alias target-suffix)]
    join-spec-instance))

(defn- joins*
  [schema db-query outer-suffix]
  (assert (vector? db-query) "Queries must be vectors.")
  (reduce (fn [{:keys [joins current-suffix] :as acc} i]
            (cond
              (keyword? i) {:joins joins :current-suffix current-suffix}
              (map? i) (let [subquery (first (vals i))
                             nsuffix (next-suffix current-suffix false)]
                         {:current-suffix nsuffix
                          :joins          (-> joins
                                              (conj (join schema i outer-suffix current-suffix))
                                              (into (:joins (joins* schema subquery current-suffix))))})
              :else acc))
          {:joins [] :current-suffix (next-suffix outer-suffix true)} db-query))

(defn- joins
  "Convert an om query into a sequence of join specifications. Follows alias sequence of columns so that join aliasing will
  match that of the columns."
  [schema om-query]
  (:joins (joins* schema (db-query schema om-query) "1")))

(defn- left-join
  "Given a join spec, emit a SQL LEFT JOIN clause."
  [spec]
  (let [{:keys [from to from-alias to-alias]} spec
        from-table (kw->sql (namespace from))
        from-alias (kw->sql (str from-table "." from-alias))
        to-table (kw->sql (namespace to))
        to-alias (kw->sql (str to-table "." to-alias))
        from-col (kw->sql (name from))
        to-col (kw->sql (name to))]
    (format "LEFT JOIN %s \"%s\" ON \"%s\".%s = \"%s\".%s"
            to-table to-alias from-alias from-col to-alias to-col)))

(defn- join-clause
  "Given the columns and join specs returns an SQL FROM clause"
  [cols join-specs]
  (let [table-name #(str/replace % #"[.].*$" "")
        top-tables (filter #(re-matches #"^[^.]*[.][^.]*$" %) (distinct (map namespace cols)))
        table-names (map table-name top-tables)
        table-clauses (map (fn [n alias] (str n " \"" alias "\"")) table-names top-tables)
        initial-from (str "FROM " (str/join "," table-clauses))
        left-joins (str/join " " (map left-join join-specs))]
    (str initial-from " " left-joins)))

(defn- col->sel
  "Convert a suffixed column selection keyword to a properly aliased SQL column selector.
  E.g. :a/b => 'a.b AS \"a/b\"'"
  [col]
  (let [nspc (kw->sql (namespace col))
        col-name (kw->sql (name col))]
    (format "\"%s\".%s AS \"%s/%s\"" nspc col-name nspc col-name)))

(defn- select-clause
  "Given a sequence of suffixed column keywords, returns the complete SQL selection items clause."
  [cols]
  (str/join ", " (map col->sel cols)))

(defn om->sql
  "Given a schema describing the Om -> SQL name mapping and join descriptions, returns an unconstrained SQL
  statement that can be used to select data in a format that can be converted to a tree by `table->tree`."
  [schema om-query]
  (let [cols (columns schema om-query)
        join-specs (joins schema om-query)]
    (str "SELECT " (select-clause cols) " " (join-clause cols join-specs))))

(defn query-ref-rules
  [schema om-query] [])

(defn query-replacement-rules
  [schema om-query]
  (let [cols (columns schema om-query)
        without-suffix (fn [kw] (keyword (table-name (namespace kw)) (name kw)))
        cols-sans-suffix (into #{} (map without-suffix cols))]
    ; TODO: remaps for pseudocolumns in relations
    ; TODO: remaps back from db -> om
    {}))


(defn query-entities
  "Returns the names of all of the tables that are being queried in the given om-query."
  [schema om-query]
  (let [cols (columns schema om-query)]
    (into #{} (map table-name cols))))

(defn query-top-key [schema om-query]
  (keyword (table-for-query schema om-query) "id"))

(defn omquery+sqlresult->tree
  "Given the database schema, an om query, and the table data returned from the
  database (using the `om->sql` query): returns the desired tree of response data."
  [schema om-query sql-result]
  (let [tree-rules {:refs     (query-ref-rules schema om-query)
                    :replace  (query-replacement-rules schema om-query)
                    :codecs   (:codecs schema)
                    :entities (query-entities schema om-query)
                    :id-key   (query-top-key schema om-query)}]
    (table->tree sql-result om-query tree-rules)))



;;; NEW IDEA:
; Each level of a query is really selecting 1+ rows of a particular table, which are then linked
; to some other table(s). If done as separate queries should issue no more queries than there are
; joins. Joins to the same table multiple times at a level could be combined. So, do a recursive
; parse of the Om query, and run the queries for each level as we go:

; [:db/id :tag/type] => SELECT id, type FROM tag
; [:db/id {:tag/name [:db/id :translation/value :translation/locale]}] =>
; WITH one-to-many config indicating name implies name_id on the tag table:
;   SELECT id, name_id FROM tag => a set of name IDs ids
;   SELECT id, value, locale FROM translation WHERE id IN (ids)
; integrate all objects into a running Om db

; Kinds of joins:
; 1. to-many via a key on the foriegn table that matches this tables id
; 2. to-one or to-many via an id in a column on this table that points to the foreign table, where the target key
; make have more than one rot
; 3. to-many with a join table, where the join table has two IDs: one pointing to this table, and one
; pointing to the foreign table
; 4. Like 3, but the foreign table is this table (e.g. parent of the same kind)

(defn- join-column [joins item]
  (let [join (get joins item)
        prop (get join :db-prop)
        prop-table (table-name prop)
        from-table (table-name (:from join))]
    (cond
      (contains? join :select) (:select join)
      (= from-table prop-table) (:from join)
      :else nil)))

(defn- top-columns*
  [schema db-query]
  (assert (vector? db-query) "Query input must be a vector")
  (reduce (fn [{:keys [cols] :as acc} item]
            (let [table (table-for-query schema db-query)
                  joins-by-dbprop (into {} (map (fn [item] [(:db-prop item) item]) (:joins schema)))]
              (cond
                (= :db/id item) (assoc acc :cols (conj cols (keyword table "id")))
                (keyword? item) (assoc acc :cols (conj cols item))
                (map? item) (let [k (first (keys item))
                                  c (join-column joins-by-dbprop k)]
                              (if c
                                (assoc acc :cols (conj cols c))
                                acc))
                :else acc)))
          {:cols []} db-query))

(defn- top-columns
  "Get all of the properly suffixed SQL columns that must be queried in order to satisfy the given om-query"
  [schema om-query]
  (:cols (top-columns* schema (db-query schema om-query))))

(defn- top-joins*
  [schema db-query]
  (assert (vector? db-query) "Query input must be a vector")
  (reduce (fn [acc item]
            (if (map? item)
              (let [joins-by-dbprop (into {} (map (fn [item] [(:db-prop item) item]) (:joins schema)))
                    k (first (keys item))
                    join (get joins-by-dbprop k)]
                (if (and join (:select join))
                  (conj acc join)
                  acc))
              acc))
          [] db-query))

(defn top-joins
  ""
  [schema om-query]
  (top-joins* schema (db-query schema om-query)))


