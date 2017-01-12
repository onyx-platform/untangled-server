(ns untangled.server.sql-spec
  (:require [untangled.server.sql :as sql]
            [untangled-spec.core :refer [specification component behavior assertions when-mocking]]
            [om.next :as om]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import (clojure.lang ExceptionInfo)))

(specification "table->tree"
  (let [sql-result [{:person/id  1 :person/name "Joe" :phone/id 1 :phone/type "home" :phone/number "555-1212"
                     :billing/id 1 :billing/cc "3333-4444-5555-1111"}
                    {:person/id 2 :person/name "Mary" :phone/id 2 :phone/type "home" :phone/number "555-1212"}
                    {:person/id 2 :person/name "Mary" :phone/id 3 :phone/type "work" :phone/number "555-9999"}]

        om-query [:db/id :person/name {:person/phone [:db/id :number :type]} {:person/billing [:db/id :billing/cc]}]

        rules {:refs     [[:person/phone :phone/id :many]
                          [:person/billing :billing/id :one]]
               :replace  {:phone/number :number
                          :phone/type   :type}
               :codecs   {:phone/type {:in  keyword
                                       :out name}}
               :entities #{:person :phone :billing}
               :id-key   :person/id}

        tree (sql/table->tree sql-result om-query rules)]
    (assertions
      tree => [{:db/id          1 :person/name "Joe" :person/phone [{:db/id 1 :type :home :number "555-1212"}]
                :person/billing {:db/id 1 :billing/cc "3333-4444-5555-1111"}}
               {:db/id 2 :person/name "Mary" :person/phone [{:db/id 2 :type :home :number "555-1212"}
                                                            {:db/id 3 :type :work :number "555-9999"}]}])))

; 1. Tag has parent col that can point to more than one parent. How do we query? Creates a tag tree.
; 2. Developer efficiency
; 3. Composition of multiple queries into a single tree result (questions have tags)

; See schema.sql for how this gets generated:
; tag/id | name/id | name/locale | name/value | tag/parent_id
; --------+---------+-------------+------------+-------------
; 1       |       1 | en          | A          |
; 2       |       2 | en          | B          |           1
; 3       |       3 | en          | C          |           4
; 3       |       3 | en          | C          |           2
; 3       |       3 | en          | C          |           1
; 4       |       4 | en          | D          |

(specification "Tag parsing"
  (let [sql-result [{:tag/id 1 :name/id 1 :name/locale "en" :name/value "A"}
                    {:tag/id 2 :name/id 2 :name/locale "en" :name/value "B" :tag/parent_id 1}
                    {:tag/id 3 :name/id 3 :name/locale "en" :name/value "C" :tag/parent_id 4}
                    {:tag/id 3 :name/id 3 :name/locale "en" :name/value "C" :tag/parent_id 2}
                    {:tag/id 3 :name/id 3 :name/locale "en" :name/value "C" :tag/parent_id 1}
                    {:tag/id 4 :name/id 4 :name/locale "en" :name/value "D"}]


        om-query [:db/id {:tag/name [:db/id :language/locale :localized-string/value]}
                  :tag/description :tag/type {:tag/parents '...}]

        rules {:refs     [[:tag/parents :tag/parent_id :many]
                          [:tag/name :name/id :many]]
               :replace  {:tag/parent_id :tag/id
                          :name/locale   :language/locale
                          :name/value    :localized-string/value}
               :codecs   {}
               :entities #{:tag :name}
               :id-key   :tag/id}

        tree (sql/table->tree sql-result om-query rules)]
    (assertions
      tree => [{:db/id       3,
                :tag/name    [{:db/id 3, :language/locale "en", :localized-string/value "C"}],
                :tag/parents [{:db/id 4, :tag/name [{:db/id 4, :language/locale "en", :localized-string/value "D"}]}
                              {:db/id       2,
                               :tag/name    [{:db/id 2, :language/locale "en", :localized-string/value "B"}],
                               :tag/parents [{:db/id 1, :tag/name [{:db/id 1, :language/locale "en", :localized-string/value "A"}]}]}
                              {:db/id 1, :tag/name [{:db/id 1, :language/locale "en", :localized-string/value "A"}]}]}
               {:db/id       2,
                :tag/name    [{:db/id 2, :language/locale "en", :localized-string/value "B"}],
                :tag/parents [{:db/id 1, :tag/name [{:db/id 1, :language/locale "en", :localized-string/value "A"}]}]}
               {:db/id 4, :tag/name [{:db/id 4, :language/locale "en", :localized-string/value "D"}]}
               {:db/id 1, :tag/name [{:db/id 1, :language/locale "en", :localized-string/value "A"}]}]))
  )

; db-prop is like an om property, but uses the proper table name. These are always psedo props, in that the idealized
; name requires a join to realize
(def schema {:omprop->dbprop {:language/locale        :translation/locale
                              :localized-string/value :translation/localized-string}
             :joins          [{:db-prop :tag/name :from :tag/name-id :to :translation/i18n-string-id :carinality :many}
                              {:db-prop :tag/description :from :tag/description-id :to :translation/i18n-string-id :carinality :many}
                              {:db-prop :tag/parents :from :tag/id :to :tag-tag/tag-id :select :tag-tag/parent-tag-id :carinality :many}]})

(defn omprops->dbprops
  "Transform an Om query to use local db table/column names"
  [translations query]
  (walk/postwalk-replace translations query))

(defn db-query
  "Convert an om query to an equivalent om format query, but with the properties renamed to match the SQL database table
  and column names"
  [schema om-query]
  (omprops->dbprops (get schema :omprop->dbprop {}) om-query))

(defn query-attributes
  "Returns the top-level attributes of the given query (without db/id) as a vector."
  [query]
  (into [] (keep (fn [i] (cond
                           (map? i) (first (keys i))
                           (= :db/id i) nil
                           :else i)) query)))

(defn table-for-query
  "Returns the table of the top-level properties of the given query as a string."
  [schema om-query]
  (let [dbquery (db-query schema om-query)
        keys (query-attributes dbquery)]
    (first (map namespace keys))))

(defn column
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

(defmacro dbg [v]
  `(let [result# ~v]
     (println '~v " => " result#)
     result#))

(defn next-suffix
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

(specification "next-suffix"
  (assertions
    "Starts at 1 when given nil"
    (next-suffix nil false) => "1"
    (next-suffix nil true) => "1"
    "Increments a number"
    (next-suffix 1 false) => "2"
    "Converts numeric inputs to strings"
    (next-suffix 1 false) => "2"
    "Increments a string"
    (next-suffix "2" false) => "3"
    (next-suffix "3" false) => "4"
    "Increments a dotted sequence's last component"
    (next-suffix "2.4" false) => "2.5"
    "Adds a new component to a number when nesting"
    (next-suffix "2" true) => "2.1"
    (next-suffix "2.1" true) => "2.1.1"
    (next-suffix "2.1.1" true) => "2.1.1.1"))

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

(defn columns
  "Get all of the properly suffixed SQL columns that must be queried in order to satisfy the given om-query"
  [schema om-query]
  (:cols (columns* schema (db-query schema om-query) "1")))

(defn join
  "Give back a join specification for a specific join element from a query. from-suffix is the
  suffix of the table that the join is going FROM, and target-suffix is the current suffix to use on the TO table."
  [schema db-join-element from-suffix target-suffix]
  (assert (map? db-join-element) "Asked to generate join info for a non-map")
  (let [join-prop (first (keys db-join-element))
        schema-by-dbprop (into {} (map (fn [item] [(:db-prop item) item]) (:joins schema)))
        join-spec (or (get schema-by-dbprop join-prop) (throw (ex-info "Om join has no join config" {:join join-prop})))
        join-spec-instance (assoc join-spec :from-alias from-suffix :to-alias target-suffix)]
    join-spec-instance))

(defn joins*
  [schema db-query outer-suffix]
  (assert (vector? db-query) "Queries must be vectors.")
  (reduce (fn [{:keys [joins current-suffix] :as acc} i]
            (cond
              (keyword? i) {:joins joins :current-suffix current-suffix}
              (map? i) (let [subquery (first (vals i))
                             nsuffix (next-suffix current-suffix false)]
                         {:current-suffix nsuffix
                          :joins          (-> joins
                                              (conj (dbg (join schema i outer-suffix current-suffix)))
                                              (into (:joins (joins* schema subquery current-suffix))))})
              :else acc
              )) {:joins [] :current-suffix (next-suffix outer-suffix true)} db-query))

(defn joins
  "Convert an om query into a sequence of join specifications. Follows alias sequence of columns so that join aliasing will
  match that of the columns."
  [schema om-query]
  (:joins (joins* schema (db-query schema om-query) "1")))

(defn kw->sql
  "Convert a keyword to the name we use in SQL. Replaces - with _."
  [kw]
  (-> kw str (str/replace #"[-]" "_")))

(defn left-join
  "Given a join spec, emit a SQL LEFT JOIN clause."
  [spec]
  (let [{:keys [from to from-alias to-alias]} spec
        from-table (kw->sql (namespace from))
        from-alias (kw->sql (str from-table "." from-alias))
        to-table (kw->sql (namespace to))
        to-alias (kw->sql (str to-table "." to-alias))
        from-col (kw->sql (name from))
        to-col (kw->sql (name to))
        ]
    (format "LEFT JOIN %s \"%s\" ON \"%s\".%s = \"%s\".%s"
            to-table to-alias from-alias from-col to-alias to-col)))

(defn join-clause
  "Given the columns and join specs returns an SQL FROM clause"
  [cols join-specs]
  (let [table-name #(str/replace % #"[.].*$" "")
        top-tables (filter #(re-matches #"^[^.]*[.][^.]*$" %) (distinct (map namespace cols)))
        table-names (map table-name top-tables)
        table-clauses (map (fn [n alias] (str n " \"" alias "\"")) table-names top-tables)
        initial-from (str "FROM " (str/join "," table-clauses))
        left-joins (str/join " " (map left-join join-specs))]
    (str initial-from " " left-joins)))

(defn col->sel
  "Convert a suffixed column selection keyword to a properly aliased SQL column selector."
  [col]
  (let [nspc (kw->sql (namespace col))
        col-name (kw->sql (name col))]
    (format "\"%s\".%s AS \"%s/%s\"" nspc col-name nspc col-name)))

(defn select-clause
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

(specification "query translation helpers"
  (component "tables-for-query"
    (assertions
      "Returns a set of table names needed to obtain the top-level properties of a query"
      (table-for-query {} [:db/id :tag/name {:boo/ya [:b]}]) => "tag"
      (table-for-query {} [:db/id :tag/type
                           {:tag/name [:db/id :language/locale :localized-string/value]}]) => "tag"
      (table-for-query {} (db-query schema [:db/id :language/locale :localized-string/value])) => "translation"
      (table-for-query {} [:db/id]) => nil))
  (component "db-query"
    (assertions
      "converts an om query to use local database table names in property namespaces"
      (db-query schema [:db/id :language/locale :localized-string/value]) => [:db/id :translation/locale :translation/localized-string]
      (db-query {:omprop->dbprop {:oma/x :dba/x :omb/y :dbb/y}} [:oma/x :omb/y]) => [:dba/x :dbb/y]))
  (component "column"
    (assertions
      "Converts a query property into an aliased selection keyword"
      (column schema :tag/type "tag" 1) => :tag.1/type
      "Converts :db/id to the proper table id with aliasing"
      (column schema :db/id "tag" 1) => :tag.1/id
      "Throws an exception if the namespace of the target does not match the table under evaluation"
      (column schema :boo/type "tag" 1) =throws=> (ExceptionInfo #".*on the wrong table.*")))
  (component "columns"
    (assertions
      "converts queries with joins into a sequence of aliased columns"
      (columns schema [:db/id :tag/type]) => [:tag.1/id :tag.1/type]
      "converts queries with om props to proper db select props with aliases"
      (columns schema [:db/id :language/locale :localized-string/value]) => [:translation.1/id :translation.1/locale :translation.1/localized-string]
      "converts queries with om joins to proper db select props with aliases"
      (columns schema [:db/id :tag/type
                       {:tag/name [:db/id :language/locale :localized-string/value]}]) => [:tag.1/id :tag.1/type
                                                                                           :translation.1.1/id
                                                                                           :translation.1.1/locale
                                                                                           :translation.1.1/localized-string]
      (columns schema [:db/id :tag/type
                       {:tag/name [:db/id :language/locale :localized-string/value]}
                       {:tag/description [:db/id :language/locale :localized-string/value]}]) => [:tag.1/id :tag.1/type
                                                                                                  :translation.1.1/id
                                                                                                  :translation.1.1/locale
                                                                                                  :translation.1.1/localized-string
                                                                                                  :translation.1.2/id
                                                                                                  :translation.1.2/locale
                                                                                                  :translation.1.2/localized-string
                                                                                                  ]))
  (component "join"
    (assertions
      "translates a single db-join to a join specification"
      (join schema {:tag/name [:db/id :translation/localized-string]} "1" "1.1") => {:db-prop    :tag/name
                                                                                     :from-alias "1"
                                                                                     :to-alias   "1.1"
                                                                                     :from       :tag/name-id
                                                                                     :to         :translation/i18n-string-id
                                                                                     :carinality :many}))
  (component "joins"
    (assertions
      "creates a sequence of join specifications for an om query"
      (joins schema [:db/id :tag/type
                     {:tag/name [:db/id :localized-string/value]}
                     {:tag/description [:db/id :localized-string/value]}]) => [{:db-prop    :tag/name
                                                                                :from-alias "1"
                                                                                :to-alias   "1.1"
                                                                                :from       :tag/name-id
                                                                                :to         :translation/i18n-string-id
                                                                                :carinality :many}
                                                                               {:db-prop    :tag/description
                                                                                :from-alias "1"
                                                                                :to-alias   "1.2"
                                                                                :from       :tag/description-id
                                                                                :to         :translation/i18n-string-id
                                                                                :carinality :many}]))
  (let [query [:db/id :tag/type
               {:tag/name [:db/id :localized-string/value]}
               {:tag/description [:db/id :localized-string/value]}
               ]
        specs (joins schema query)
        cols (columns schema query)]
    (component "select-clause"
      (assertions
        "converts column keywords to a select clause"
        (select-clause cols) => "\"tag.1\".id AS \"tag.1/id\", \"tag.1\".type AS \"tag.1/type\", \"translation.1.1\".id AS \"translation.1.1/id\", \"translation.1.1\".localized_string AS \"translation.1.1/localized_string\", \"translation.1.2\".id AS \"translation.1.2/id\", \"translation.1.2\".localized_string AS \"translation.1.2/localized_string\""))
    (component "join-clause"
      (assertions
        "converts a sequence of joins specifications to an SQL FROM/JOIN clause with appropriate aliases"
        (join-clause cols specs) => (str "FROM tag \"tag.1\" "
                                         "LEFT JOIN translation \"translation.1.1\" ON \"tag.1\".name_id = \"translation.1.1\".i18n_string_id "
                                         "LEFT JOIN translation \"translation.1.2\" ON \"tag.1\".description_id = \"translation.1.2\".i18n_string_id")))
    (component "om->sql"
      (assertions
        "can convert an om query to SQL"
        (om->sql schema query) => "SELECT \"tag.1\".id AS \"tag.1/id\", \"tag.1\".type AS \"tag.1/type\", \"translation.1.1\".id AS \"translation.1.1/id\", \"translation.1.1\".localized_string AS \"translation.1.1/localized_string\", \"translation.1.2\".id AS \"translation.1.2/id\", \"translation.1.2\".localized_string AS \"translation.1.2/localized_string\" FROM tag \"tag.1\" LEFT JOIN translation \"translation.1.1\" ON \"tag.1\".name_id = \"translation.1.1\".i18n_string_id LEFT JOIN translation \"translation.1.2\" ON \"tag.1\".description_id = \"translation.1.2\".i18n_string_id"
        ))))
