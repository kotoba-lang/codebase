(ns kotoba.codebase.typed-eval
  "Execute a typed definition by hydrating its KIR closure from the store.

  `evaluator` interprets `semantic-code`'s own IR. This runs the compiler's
  KIR through `kotoba.kir/execute` -- the executable language oracle the AOT
  and JIT backends qualify against -- so what the codebase runs and what a
  target compiles are the same representation rather than two implementations
  of an idea.

  Assembly is where the content-addressing shows: the module handed to the
  interpreter has no source names in it at all. Every function is named by its
  own CID, and every call was rewritten to a CID reference at authoring time,
  so the assembled module is a pure function of the hashes that were reachable.

  Effects are not implemented here and not silently permitted either. A
  `typed-cap-call` dispatcher may be injected by a caller that has a provider
  registry and a policy; without one, a definition that performs a capability
  call traps rather than proceeding."
  (:require [clojure.string :as str]
            [kotoba.codebase.ir :as ir]
            [kotoba.codebase.store :as store]
            [kotoba.codebase.typed-code :as typed]
            [kotoba.kir :as kir]))

(def default-fuel 100000)
(def max-closure-blocks 4096)

(defn- fail! [problem data]
  (throw (ex-info (name problem) (assoc data :problem problem))))

;; ---------------------------------------------------------------------------
;; Decoding

(defn- decode-keyword [s]
  (if-let [index (str/index-of s "/")]
    (keyword (subs s 0 index) (subs s (inc index)))
    (keyword s)))

(declare decode-form)

(defn- decode-tagged [[tag payload] resolve-reference]
  (case tag
    "nil" nil
    "bool" payload
    "int" (bigint payload)
    "f32" (Float/intBitsToFloat (int payload))
    "f64" (Double/longBitsToDouble (long payload))
    "str" payload
    "kw" (decode-keyword payload)
    "sym" (symbol payload)
    "vec" (mapv #(decode-form % resolve-reference) payload)
    "list" (apply list (map #(decode-form % resolve-reference) payload))
    "set" (into #{} (map #(decode-form % resolve-reference)) payload)
    "map" (into {} (map (fn [[k v]] [(decode-form k resolve-reference)
                                     (decode-form v resolve-reference)]))
                payload)
    (fail! :typed-eval/unknown-canonical-tag {:tag tag})))

(defn- decode-form [form resolve-reference]
  (cond
    (and (map? form) (= "reference" (get form "op")))
    (resolve-reference {:kind :definition :cid (ir/link->cid (get form "cid"))})

    (and (map? form) (= "recursive-reference" (get form "op")))
    (resolve-reference {:kind :group-member :index (get form "index")})

    (vector? form) (decode-tagged form resolve-reference)

    :else (fail! :typed-eval/malformed-canonical-form {:form form})))

;; ---------------------------------------------------------------------------
;; Names
;;
;; Deterministic and derived only from hashes: two independently assembled
;; modules over the same closure produce byte-identical function names.

(defn decode-view-form
  "Decode a canonical body for READING rather than for execution.

  Execution resolves a reference by emitting the callee as another function in
  the assembled module; a reader wants the name it knows instead. Same decoder,
  different resolver -- which is only possible because the reference is a link
  in the data rather than a name baked into it."
  [form {:keys [name-of member-name]}]
  (decode-form form
               (fn [{:keys [kind cid index]}]
                 (case kind
                   :definition (name-of cid)
                   :group-member (member-name index)))))

(defn definition-symbol [cid] (symbol (str "kotoba_def_" cid)))
(defn member-symbol [group-cid index] (symbol (str "kotoba_grp_" group-cid "_" index)))

;; ---------------------------------------------------------------------------
;; Interface

(defn- interface-of [root cid]
  (let [block (store/get-block root cid)]
    (when-not (= "kotoba.typed-interface.v1" (get block "schema"))
      (fail! :typed-eval/not-an-interface {:cid cid}))
    {:arity (get block "arity")
     :param-types (mapv #(decode-form % (fn [_] (fail! :typed-eval/reference-in-type {})))
                        (get block "paramTypes"))
     :result (decode-form (get block "result")
                          (fn [_] (fail! :typed-eval/reference-in-type {})))
     :effects (into #{} (map keyword) (get block "effects"))
     :schemas (into {} (map (fn [[name definition]]
                              [(decode-form name (fn [_] (fail! :typed-eval/reference-in-type {})))
                               (decode-form definition (fn [_] (fail! :typed-eval/reference-in-type {})))]))
                    (get block "schemas"))}))

(defn- params-for [arity]
  (mapv #(symbol (str "k" %)) (range arity)))

;; ---------------------------------------------------------------------------
;; Assembly

(defn assemble
  "Hydrate the closure rooted at CID and build one KIR module for it.

  Returns `{:kir :entry :interface}`. Every reachable definition becomes a
  function; nothing outside the closure can be reached, because there is no
  name left in the module that could name it."
  [root cid]
  (let [functions (volatile! [])
        schemas (volatile! {})
        formats (volatile! #{})
        seen (volatile! #{})
        entry (volatile! nil)]
    (letfn [(emit-definition [cid]
              (when-not (contains? @seen cid)
                (vswap! seen conj cid)
                (when (> (count @seen) max-closure-blocks)
                  (fail! :typed-eval/closure-too-large {:limit max-closure-blocks}))
                (let [block (store/get-block root cid)]
                  (condp = (get block "schema")
                    typed/schema
                    (let [interface (interface-of root (ir/link->cid (get block "interface")))
                          name (definition-symbol cid)]
                      (vswap! formats conj (get block "irFormat"))
                      (vswap! schemas into (:schemas interface))
                      ;; Reserve the slot before decoding: a self-referential
                      ;; closure would otherwise recurse forever through
                      ;; `emit-definition` while resolving its own body.
                      (let [body (decode-form (get block "body") resolve-reference)]
                        (vswap! functions conj
                                {:name name
                                 :params (params-for (:arity interface))
                                 :param-types (:param-types interface)
                                 :result (:result interface)
                                 :effects (:effects interface)
                                 :body body}))
                      {:name name :interface interface})

                    typed/member-schema
                    (emit-group (ir/link->cid (get block "group")))

                    (fail! :typed-eval/not-a-typed-definition
                           {:cid cid :schema (get block "schema")})))))

            (emit-group [group-cid]
              (when-not (contains? @seen group-cid)
                (vswap! seen conj group-cid)
                (let [block (store/get-block root group-cid)]
                  (when-not (= typed/group-schema (get block "schema"))
                    (fail! :typed-eval/not-a-typed-group {:cid group-cid}))
                  (vswap! formats conj (get block "irFormat"))
                  (let [members (get block "members")]
                    (doseq [[index member] (map-indexed vector members)]
                      (let [interface (interface-of root (ir/link->cid (get member "interface")))
                            body (decode-form (get member "body")
                                              (group-resolver group-cid))]
                        (vswap! schemas into (:schemas interface))
                        (vswap! functions conj
                                {:name (member-symbol group-cid index)
                                 :params (params-for (:arity interface))
                                 :param-types (:param-types interface)
                                 :result (:result interface)
                                 :effects (:effects interface)
                                 :body body})))))))

            (group-resolver [group-cid]
              (fn [{:keys [kind cid index]}]
                (case kind
                  :definition (do (emit-definition cid) (definition-symbol cid))
                  :group-member (member-symbol group-cid index))))

            (resolve-reference [{:keys [kind cid]}]
              (case kind
                :definition (do (emit-definition cid) (definition-symbol cid))
                (fail! :typed-eval/recursive-reference-outside-group {})))]

      (let [block (store/get-block root cid)
            member? (= typed/member-schema (get block "schema"))
            entry-name (if member?
                         (member-symbol (ir/link->cid (get block "group")) (get block "index"))
                         (definition-symbol cid))
            interface (interface-of root (ir/link->cid (get block "interface")))]
        (if member?
          (emit-group (ir/link->cid (get block "group")))
          (emit-definition cid))
        (vreset! entry entry-name)
        (when-not (= 1 (count @formats))
          (fail! :typed-eval/mixed-ir-formats {:formats (vec @formats)}))
        {:entry entry-name
         :interface interface
         :kir {:format (keyword (subs (first @formats) 1))
               :entry entry-name
               :exports [entry-name]
               :schemas @schemas
               :functions (vec @functions)}}))))

;; ---------------------------------------------------------------------------
;; Execution

(defn invoke
  "Execute the definition at CID with ARGS.

  Fuel is bounded by the KIR interpreter itself; a capability call without an
  injected dispatcher traps as denied rather than being skipped."
  ([root cid args] (invoke root cid args {}))
  ([root cid args {:keys [fuel typed-cap-call cap-call]
                   :or {fuel default-fuel}}]
   (let [{:keys [kir entry interface]} (assemble root cid)]
     (when-not (= (count args) (:arity interface))
       (fail! :typed-eval/arity-mismatch
              {:expected (:arity interface) :actual (count args)}))
     {:cid cid
      :entry entry
      :effects (:effects interface)
      :value (kir/execute kir entry (vec args)
                          (cond-> {:fuel fuel}
                            typed-cap-call (assoc :typed-cap-call typed-cap-call)
                            cap-call (assoc :cap-call cap-call)))})))
