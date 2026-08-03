(ns kotoba.codebase.fetch-test
  (:require [cbor.core :as cbor]
            [clojure.test :refer [deftest is testing]]
            [kotoba.codebase.authoring :as authoring]
            [kotoba.codebase.evaluator :as evaluator]
            [kotoba.codebase.fetch :as fetch]
            [kotoba.codebase.store :as store]))

(defn- temp-store []
  (.toFile (java.nio.file.Files/createTempDirectory
            "kotoba-fetch-"
            (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-tree [root]
  (doseq [f (reverse (file-seq root))] (.delete ^java.io.File f)))

(defn- serving
  "A fetch source backed by another local store."
  [remote]
  (fn [cid]
    (try (cbor/encode (store/get-block remote cid))
         (catch clojure.lang.ExceptionInfo _ nil))))

(deftest hydrates-a-closure-from-a-remote-source-and-runs-it
  (let [remote (temp-store) local (temp-store)]
    (try
      (store/initialize! remote)
      (store/initialize! local)
      (let [committed (authoring/update-namespace!
                       remote "scratch" '[(defn double [x] (* x 2))
                                          (defn quadruple [x] (double (double x)))])
            cid (get-in committed [:bindings "quadruple"])
            result (fetch/hydrate! local [cid] {:fetch-block (serving remote)})]
        (is (true? (:complete? result)))
        (testing "the definition runs locally with no source and no namespace"
          (is (= 12 (:value (evaluator/invoke local cid [3]))))))
      (finally (delete-tree remote) (delete-tree local)))))

(deftest rejects-bytes-that-do-not-hash-to-the-requested-cid
  (let [remote (temp-store) local (temp-store)]
    (try
      (store/initialize! remote)
      (store/initialize! local)
      (let [committed (authoring/update-namespace!
                       remote "scratch" '[(defn double [x] (* x 2))])
            cid (get-in committed [:bindings "double"])
            lying (fn [_] (cbor/encode {"schema" "not-what-you-asked-for"}))]
        (is (= :codebase/fetched-cid-mismatch
               (:problem (ex-data (try (fetch/hydrate! local [cid] {:fetch-block lying})
                                       (catch clojure.lang.ExceptionInfo e e)))))))
      (finally (delete-tree remote) (delete-tree local)))))

(deftest reports-what-the-source-did-not-have
  (let [remote (temp-store) local (temp-store)]
    (try
      (store/initialize! remote)
      (store/initialize! local)
      (let [committed (authoring/update-namespace!
                       remote "scratch" '[(defn double [x] (* x 2))
                                          (defn quadruple [x] (double (double x)))])
            cid (get-in committed [:bindings "quadruple"])
            dependency (get-in committed [:bindings "double"])
            partial-source (fn [wanted]
                             (when-not (= wanted dependency)
                               ((serving remote) wanted)))
            result (fetch/hydrate! local [cid] {:fetch-block partial-source})]
        (is (false? (:complete? result)))
        (is (= [dependency] (:missing result)))
        (testing "an incomplete closure fails closed at evaluation, not silently"
          (is (= :codebase/block-not-found
                 (:problem (ex-data (try (evaluator/invoke local cid [3])
                                         (catch clojure.lang.ExceptionInfo e e))))))))
      (finally (delete-tree remote) (delete-tree local)))))

(deftest bounds-the-number-of-blocks-one-request-can-pull
  (let [remote (temp-store) local (temp-store)]
    (try
      (store/initialize! remote)
      (store/initialize! local)
      (let [committed (authoring/update-namespace!
                       remote "scratch" '[(defn a [x] (+ x 1))
                                          (defn b [x] (a (a x)))
                                          (defn c [x] (b (b x)))])
            cid (get-in committed [:bindings "c"])]
        (is (= :codebase/fetch-budget-exceeded
               (:problem (ex-data (try (fetch/hydrate! local [cid]
                                                       {:fetch-block (serving remote)
                                                        :max-blocks 1})
                                       (catch clojure.lang.ExceptionInfo e e)))))))
      (finally (delete-tree remote) (delete-tree local)))))
