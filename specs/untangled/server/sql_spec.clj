(ns ^:focused untangled.server.sql-spec
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


        om-query [:db/id :tag/type
                  {:tag/name [:db/id :language/locale :localized-string/value]}
                  {:tag/parents '...}]

        rules {:refs     [[:tag/parents :tag/parent_id :many]
                          [:tag/name :name/id :many]
                          [:tag/description :description/id :many]]
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
                              {:db/id    1,
                               :tag/name [{:db/id 1, :language/locale "en", :localized-string/value "A"}]}]}
               {:db/id       2,
                :tag/name    [{:db/id 2, :language/locale "en", :localized-string/value "B"}],
                :tag/parents [{:db/id    1,
                               :tag/name [{:db/id 1, :language/locale "en", :localized-string/value "A"}]}]}
               {:db/id 4, :tag/name [{:db/id 4, :language/locale "en", :localized-string/value "D"}]}
               {:db/id    1,
                :tag/name [{:db/id 1, :language/locale "en", :localized-string/value "A"}]}])))


; db-prop is like an om property, but uses the proper table name. These are always psedo props, in that the idealized
; name requires a join to realize
(def schema {:omprop->dbprop {:language/locale        :translation/locale
                              :localized-string/value :translation/localized-string}
             :joins          [{:db-prop :tag/name :from :tag/name-id :to :translation/i18n-string-id :carinality :many}
                              {:db-prop :tag/description :from :tag/description-id :to :translation/i18n-string-id :carinality :many}
                              {:db-prop :tag/category :from :category/tag-id :to :tag/id :carinality :many}
                              {:db-prop :tag/parents :from :tag/id :to :tag-tag/tag-id :select :tag-tag/parent-tag-id :carinality :many}]})

(defmacro dbg [v]
  `(let [result# ~v]
     (println '~v " => " result#)
     result#))

(specification "next-suffix"
  (assertions
    "Starts at 1 when given nil"
    (#'sql/next-suffix nil false) => "1"
    (#'sql/next-suffix nil true) => "1"
    "Increments a number"
    (#'sql/next-suffix 1 false) => "2"
    "Converts numeric inputs to strings"
    (#'sql/next-suffix 1 false) => "2"
    "Increments a string"
    (#'sql/next-suffix "2" false) => "3"
    (#'sql/next-suffix "3" false) => "4"
    "Increments a dotted sequence's last component"
    (#'sql/next-suffix "2.4" false) => "2.5"
    "Adds a new component to a number when nesting"
    (#'sql/next-suffix "2" true) => "2.1"
    (#'sql/next-suffix "2.1" true) => "2.1.1"
    (#'sql/next-suffix "2.1.1" true) => "2.1.1.1"))

(specification "table-name"
  (assertions
    "returns the table name as the namespace of the keyword"
    (sql/table-name :t/a) => "t"
    "removes join positioning suffixes"
    (sql/table-name :t.1/a) => "t"
    (sql/table-name :t.1.1/a) => "t"
    (sql/table-name :t.1.2.3.4/a) => "t"
    (sql/table-name :t.5/a) => "t"))

(specification "query translation helpers"
  (component "tables-for-query"
    (assertions
      "Returns a set of table names needed to obtain the top-level properties of a query"
      (#'sql/table-for-query {} [:db/id :tag/name {:boo/ya [:b]}]) => "tag"
      (#'sql/table-for-query {} [:db/id :tag/type
                                 {:tag/name [:db/id :language/locale :localized-string/value]}]) => "tag"
      (#'sql/table-for-query {} (#'sql/db-query schema [:db/id :language/locale :localized-string/value])) => "translation"
      (#'sql/table-for-query {} [:db/id]) => nil))
  (component "db-query"
    (assertions
      "converts an om query to use local database table names in property namespaces"
      (#'sql/db-query schema [:db/id :language/locale :localized-string/value]) => [:db/id :translation/locale :translation/localized-string]
      (#'sql/db-query {:omprop->dbprop {:oma/x :dba/x :omb/y :dbb/y}} [:oma/x :omb/y]) => [:dba/x :dbb/y]))
  (component "column"
    (assertions
      "Converts a query property into an aliased selection keyword"
      (#'sql/column schema :tag/type "tag" 1) => :tag.1/type
      "Converts :db/id to the proper table id with aliasing"
      (#'sql/column schema :db/id "tag" 1) => :tag.1/id
      "Throws an exception if the namespace of the target does not match the table under evaluation"
      (#'sql/column schema :boo/type "tag" 1) =throws=> (ExceptionInfo #".*on the wrong table.*")))
  (component "columns"
    (let [om-query [:db/id :tag/type
                    {:tag/name [:db/id :language/locale :localized-string/value]}
                    {:tag/description [:db/id :language/locale :localized-string/value]}
                    {:tag/parents '...}]]
      (assertions
        "boo"
        (#'sql/columns schema om-query) => [:tag.1/id :tag.1/type]
        "converts queries with joins into a sequence of aliased columns"
        (#'sql/columns schema [:db/id :tag/type]) => [:tag.1/id :tag.1/type]
        "converts queries with om props to proper db select props with aliases"
        (#'sql/columns schema [:db/id :language/locale :localized-string/value]) => [:translation.1/id :translation.1/locale :translation.1/localized-string]
        "converts queries with om joins to proper db select props with aliases"
        (#'sql/columns schema [:db/id :tag/type
                               {:tag/name [:db/id :language/locale :localized-string/value]}]) => [:tag.1/id :tag.1/type
                                                                                                   :translation.1.1/id
                                                                                                   :translation.1.1/locale
                                                                                                   :translation.1.1/localized-string]
        (#'sql/columns schema [:db/id :tag/type
                               {:tag/name [:db/id :language/locale :localized-string/value]}
                               {:tag/description [:db/id :language/locale :localized-string/value]}]) => [:tag.1/id :tag.1/type
                                                                                                          :translation.1.1/id
                                                                                                          :translation.1.1/locale
                                                                                                          :translation.1.1/localized-string
                                                                                                          :translation.1.2/id
                                                                                                          :translation.1.2/locale
                                                                                                          :translation.1.2/localized-string])))

  (component "join"
    (assertions
      "translates a single db-join to a join specification"
      (#'sql/join schema {:tag/name [:db/id :translation/localized-string]} "1" "1.1") => {:db-prop    :tag/name
                                                                                           :from-alias "1"
                                                                                           :to-alias   "1.1"
                                                                                           :from       :tag/name-id
                                                                                           :to         :translation/i18n-string-id
                                                                                           :carinality :many}))
  (component "joins"
    (assertions
      "creates a sequence of join specifications for an om query"
      (#'sql/joins schema [:db/id :tag/type
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
               {:tag/description [:db/id :localized-string/value]}]
        specs (#'sql/joins schema query)
        cols (#'sql/columns schema query)]
    (component "select-clause"
      (assertions
        "converts column keywords to a select clause"
        (#'sql/select-clause cols) => "\"tag.1\".id AS \"tag.1/id\", \"tag.1\".type AS \"tag.1/type\", \"translation.1.1\".id AS \"translation.1.1/id\", \"translation.1.1\".localized_string AS \"translation.1.1/localized_string\", \"translation.1.2\".id AS \"translation.1.2/id\", \"translation.1.2\".localized_string AS \"translation.1.2/localized_string\""))
    (component "join-clause"
      (assertions
        "converts a sequence of joins specifications to an SQL FROM/JOIN clause with appropriate aliases"
        (#'sql/join-clause cols specs) => (str "FROM tag \"tag.1\" "
                                               "LEFT JOIN translation \"translation.1.1\" ON \"tag.1\".name_id = \"translation.1.1\".i18n_string_id "
                                               "LEFT JOIN translation \"translation.1.2\" ON \"tag.1\".description_id = \"translation.1.2\".i18n_string_id")))
    (component "om->sql"
      (assertions
        "can convert an om query to SQL"
        (sql/om->sql schema query) =>
        (str "SELECT \"tag.1\".id AS \"tag.1/id\", \"tag.1\".type AS \"tag.1/type\", \"translation.1.1\".id AS \"translation.1.1/id\", \"translation.1.1\".localized_string AS \"translation.1.1/localized_string\", \"translation.1.2\".id AS \"translation.1.2/id\", \"translation.1.2\".localized_string AS \"translation.1.2/localized_string\" "
             "FROM tag \"tag.1\" "
             "LEFT JOIN translation \"translation.1.1\" ON \"tag.1\".name_id = \"translation.1.1\".i18n_string_id "
             "LEFT JOIN translation \"translation.1.2\" ON \"tag.1\".description_id = \"translation.1.2\".i18n_string_id")))))


(specification "top-columns"
  (assertions
    "can find the correct columns to select for the top properties with a forward join"
    (#'sql/top-columns schema [:db/id {:tag/name [:db/id :translation/value]}]) => [:tag/id :tag/name-id]
    "does not pull columns for reverse joins"
    (#'sql/top-columns schema [:db/id {:tag/category [:db/id :category/name]}]) => [:tag/id]
    "pulls the column for to-many when there is a join tables"
    (#'sql/top-columns schema [:db/id :tag/type {:tag/parents '...}]) => [:tag/id :tag/type :tag-tag/parent-tag-id]))

(specification "top-joins"
  (assertions
    "gives joins for join-table ID extraction"
    (sql/top-joins schema [:db/id :tag/type {:tag/parents '...}]) => [{:db-prop :tag/parents :from :tag/id :to :tag-tag/tag-id :select :tag-tag/parent-tag-id :carinality :many}]))


