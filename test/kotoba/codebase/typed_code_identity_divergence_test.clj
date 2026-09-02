(ns kotoba.codebase.typed-code-identity-divergence-test
  "Two algorithms answer `what is this definition`, and this suite pins that
  they answer differently.

  ADR-2608550000 (superproject, accepted 2026-08-04) converged L0 identity on
  the compiler's checked KIR through `kotoba.codebase.typed-code`, and named
  `two answers to one question` as the defect it was fixing. Measured
  2026-09-02: `lang/code-identity.edn` in kotoba-lang/kotoba-lang names
  `kotoba.kir.definition-identity` (payload version 2, canonical DAG-CBOR over
  typed-kir / profile-version / desugar-contract-version / effect-row /
  interface / direct-definition-dependencies, 10 frozen vectors, JVM and
  ClojureScript byte-identical) as THE implementation -- and `typed-code`
  does not call it. It hashes its own canonical form (`kotoba.typed-definition.v1`,
  own alpha-normalization, own `canonical-form`, interface as a separate block)
  with different bytes. The two CIDs for one definition differ.

  The direction recorded in `lang/code-identity.edn :identity-implementations`
  is that typed-code adopts `kotoba.kir.definition-identity` as its hashing
  core under a versioned migration. That migration is NOT this change: it
  moves every stored typed-code CID, and whether any are persisted outside
  tests could not be verified (local stores and kotobase providers are not
  enumerable). Until it lands, this test is the record: if either side changes
  so that the two agree, or so that the effect encoding below changes, this
  suite must be revisited on purpose, not silently.

  The second thing pinned here is how each side carries an effect row. The
  compiler emits `[:cap/call <id>]` wire vectors; typed-code stringifies
  whatever it is handed (`stable-name` falls through to `str`), so a compiler
  row is sealed as the string \"[:cap/call 9]\"; kotoba-kir refuses the wire
  row and requires the named-operation keyword row, reachable only through
  `effect-row-from-hir` with the catalog's id->name."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.codebase.typed-code :as typed]
            [kotoba.kir.definition-identity :as di]))

(def ^:private double-fn
  {:name 'double :params '[x] :param-types [:i64] :result :i64 :effects #{}
   :body '(* x 2)})

(def ^:private emit-fn
  "A capability call the way the compiler reports it: the wire row."
  {:name 'emit :params '[x] :param-types [:i64] :result :i64
   :effects #{[:cap/call 9]}
   :body '(typed-cap-call 9 :i64 :i64 x)})

(defn- kir [functions]
  {:format :kotoba.kir/v3 :exports (mapv :name functions) :schemas nil
   :functions (vec functions)})

(defn- typed-cid [function]
  (get-in (typed/compile-module (kir [function])) [:definitions (:name function) :cid]))

(defn- typed-effects [function]
  (get-in (typed/compile-module (kir [function]))
          [:definitions (:name function) :interface-block "effects"]))

(defn- kir-definition
  "The same function presented to kotoba.kir.definition-identity. Its typed
  KIR must be an IR node with :op; the compiler's function map is not one, so
  it is wrapped as a function node here."
  [function effect-row]
  {:definition/profile-version 4
   :definition/desugar-contract-version 1
   :definition/kir {:op :kir/function
                    :params (:params function)
                    :param-types (:param-types function)
                    :result (:result function)
                    :body (:body function)}
   :definition/effect-row effect-row
   :definition/interface {:arity (count (:params function))
                          :params (:param-types function)
                          :result (:result function)}
   :definition/dependencies []})

(deftest the-two-identity-algorithms-give-different-cids-for-one-definition
  (testing "(defn double [x] (* x 2)) -- pure"
    (let [a (typed-cid double-fn)
          b (di/definition-cid (kir-definition double-fn #{}))]
      (println "  typed-code            double:" a)
      (println "  kir.definition-identity double:" b)
      (is (string? a))
      (is (string? b))
      (is (not= a b)
          "ADR-2608550000 gap: two answers to `what is this definition`. If this
           becomes equal, the migration in lang/code-identity.edn
           :identity-implementations :direction has landed -- update the
           contract, and make this an equality test on purpose.")))
  (testing "a capability call -- the effect row is where the two disagree most"
    (let [a (typed-cid emit-fn)
          b (di/definition-cid
             (kir-definition emit-fn
                             (di/effect-row-from-hir {:effects (:effects emit-fn)}
                                                     {:id->name {9 :log/write}})))]
      (println "  typed-code            emit:" a)
      (println "  kir.definition-identity emit:" b)
      (is (not= a b)))))

(deftest the-two-sides-carry-an-effect-row-in-different-vocabularies
  (testing "typed-code seals whatever it is handed, as a string"
    (is (= ["log/write"] (typed-effects (assoc emit-fn :effects #{:log/write})))
        "a keyword name is sealed as its name")
    (is (= ["[:cap/call 9]"] (typed-effects emit-fn))
        "the compiler's wire row is sealed as the printed vector -- not refused,
         not translated. typed-eval reads it back with (map keyword), which
         yields the keyword :[:cap/call 9] and can never match an allowed
         effect. Pinned so the encoding cannot change silently."))
  (testing "kotoba-kir refuses the wire row and seals the named operation"
    (is (= "definition effect row members must be keywords"
           (:message (di/definition-error (kir-definition emit-fn (:effects emit-fn))))))
    (is (= #{:log/write}
           (di/effect-row-from-hir {:effects (:effects emit-fn)} {:id->name {9 :log/write}})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"effect row wire id has no catalog name: \[:cap/call 9\]"
                          (di/effect-row-from-hir {:effects (:effects emit-fn)} {:id->name {}}))
        "and never guesses a name for an id the catalog does not have")))
