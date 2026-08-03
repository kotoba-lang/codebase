(ns kotoba.codebase.render
  "Render a stored definition back into readable Kotoba source.

  In a content-addressed codebase this is what `view` and `edit` are made of:
  the definition is bytes, and source is a PROJECTION of those bytes chosen for
  a reader. Two facts follow directly from how identity is defined and are
  visible here:

  - binder names are not recoverable, because they were never hashed. Rendered
    binders are generated, and the rendered source is therefore alpha-
    equivalent to -- not textually identical with -- whatever was typed;
  - a dependency renders as whatever name the reader's namespace selects for
    that CID, or as its hash when no name does. The same definition legitimately
    renders differently for two readers, and neither rendering is more correct."
  (:require [ipld.value :as value]
            [kotoba.codebase.ir :as ir]
            [kotoba.codebase.semantic-code :as semantic]
            [kotoba.codebase.store :as store]))

(defn- fail! [problem data]
  (throw (ex-info (name problem) (assoc data :problem problem))))

(defn short-cid
  "The abbreviated form used when a CID has no selected name."
  ([cid] (short-cid cid 10))
  ([cid length] (str "#" (subs cid 0 (min (count cid) length)))))

(defn- binder [index]
  (let [letter (char (+ (int \a) (mod index 26)))
        cycle (quot index 26)]
    (symbol (if (zero? cycle) (str letter) (str letter cycle)))))

(declare render-node)

(defn- render-seq [nodes context]
  (mapv #(render-node % context) nodes))

(defn- reference-name [cid {:keys [names]}]
  (if-let [name (get names cid)]
    (symbol name)
    (symbol (short-cid cid))))

(defn- render-node [node {:keys [binders] :as context}]
  (when-not (map? node)
    (fail! :codebase/malformed-ir {:node node}))
  (case (get node "op")
    "literal" (let [v (value/form->value (get node "value"))]
                (cond
                  (value/float64? v) (value/float64-value v)
                  ;; A quoted collection or symbol is data, and rendering it
                  ;; bare would turn it back into an expression on re-read.
                  (or (symbol? v) (seq? v) (list? v)) (list 'quote v)
                  :else v))

    "local" (let [position (- (count binders) 1 (get node "index"))]
              (if (and (not (neg? position)) (< position (count binders)))
                (nth binders position)
                (symbol (str "%unbound-" (get node "index")))))

    "intrinsic" (symbol (subs (get node "id") (count "kotoba.intrinsic/v1/")))
    "reference" (reference-name (ir/link->cid (get node "cid")) context)
    "recursive-reference" (let [group (:group-names context)
                                index (get node "index")]
                            (if (and group (< -1 index (count group)))
                              (symbol (nth group index))
                              (symbol (str "%recursive-" index))))

    "vector" (render-seq (get node "items") context)
    "set" (set (render-seq (get node "items") context))
    "map" (into {} (map (fn [[k v]] [(render-node k context) (render-node v context)]))
                (get node "entries"))

    "if" (cons 'if (render-seq (get node "args") context))
    "do" (cons 'do (render-seq (get node "body") context))
    "and" (cons 'and (render-seq (get node "args") context))
    "or" (cons 'or (render-seq (get node "args") context))
    "when" (list* 'when (render-node (get node "test") context)
                  (render-seq (get node "body") context))

    "let"
    (let [[pairs context]
          (reduce (fn [[pairs ctx] binding]
                    (let [value (render-node (get binding "value") ctx)
                          name (binder (count (:binders ctx)))]
                      [(conj pairs name value) (update ctx :binders conj name)]))
                  [[] context]
                  (get node "bindings"))]
      (list* 'let pairs (render-seq (get node "body") context)))

    "fn"
    (let [params (mapv (fn [i] (binder (+ (count binders) i)))
                       (range (count (get node "params"))))
          context (update context :binders into params)]
      (list* 'fn params (render-seq (get node "body") context)))

    "call" (cons (render-node (get node "callee") context)
                 (render-seq (get node "args") context))

    (fail! :codebase/unsupported-ir-op {:op (get node "op")})))

(defn- names-for
  "CID -> selected name, from a namespace head when one is given."
  [root namespace]
  (if-let [head (and namespace (store/head root namespace))]
    (into {} (map (fn [[name cid]] [cid name])) (:bindings (store/namespace-view root head)))
    {}))

(defn- definition-form [name ir context]
  (if (= "fn" (get ir "op"))
    (let [rendered (render-node ir context)]
      (list* 'defn (symbol name) (second rendered) (drop 2 rendered)))
    (list 'def (symbol name) (render-node ir context))))

(defn view
  "Render the definition at CID as a top-level form.

  NAME is what the caller wants it called; when omitted the short hash is used,
  which is exactly what a definition with no selected name IS called."
  ([root cid] (view root cid {}))
  ([root cid {:keys [namespace name names]}]
   (let [block (store/get-block root cid)
         names (or names (names-for root namespace))
         name (or name (get names cid) (short-cid cid))
         context {:binders [] :names names}]
     (condp = (ir/block-kind block)
       semantic/schema
       {:cid cid :name name :form (definition-form name (get block "ir") context)}

       "kotoba.recursive-member.v1"
       (let [group-cid (ir/link->cid (get block "group"))
             group (store/get-block root group-cid)
             index (get block "index")
             members (get group "members")
             ;; Sibling members have no names of their own here; a stable
             ;; placeholder keeps the rendering readable and honest about that.
             group-names (mapv (fn [i] (if (= i index) name (str name "-member-" i)))
                               (range (count members)))
             context (assoc context :group-names group-names)]
         (when-not (and (integer? index) (< -1 index (count members)))
           (fail! :codebase/recursive-member-out-of-range {:cid cid :index index}))
         {:cid cid :name name :group group-cid
          :form (definition-form name (get (nth members index) "ir") context)})

       (fail! :codebase/not-viewable-block {:cid cid :schema (ir/block-kind block)})))))

(defn view-namespace
  "Render every definition a namespace selects, in name order."
  [root namespace]
  (let [head (or (store/head root namespace)
                 (fail! :codebase/head-not-found {:namespace namespace}))
        bindings (:bindings (store/namespace-view root head))
        names (into {} (map (fn [[name cid]] [cid name])) bindings)]
    {:namespace namespace
     :head head
     :definitions (mapv (fn [[name cid]] (view root cid {:name name :names names}))
                        bindings)}))
