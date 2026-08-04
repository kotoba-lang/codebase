(ns kotoba.codebase.publication-test
  "Signed namespace heads.

  Each test states an attack the model must survive, because a trust model that
  is only exercised on the happy path has not been tested at all."
  (:require [clojure.test :refer [deftest is testing]]
            [ed25519.core :as ed]
            [kotoba.codebase.authoring :as authoring]
            [kotoba.codebase.publication :as publication]
            [kotoba.codebase.store :as store]))

(defn- temp-store []
  (.toFile (java.nio.file.Files/createTempDirectory
            "kotoba-publication-" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-tree [root]
  (doseq [f (reverse (file-seq root))] (.delete ^java.io.File f)))

(defn- seed [n]
  (byte-array (map unchecked-byte (repeat 32 n))))

(defn- with-stores
  "A publisher store and a follower store, plus the publisher's DID."
  [body-fn]
  (let [publisher (temp-store) follower (temp-store)]
    (try
      (store/initialize! publisher)
      (store/initialize! follower)
      (body-fn publisher follower)
      (finally (delete-tree publisher) (delete-tree follower)))))

(defn- publish-definitions! [root namespace forms seed-bytes]
  (authoring/update-namespace! root namespace forms)
  (publication/publish! root namespace seed-bytes))

(defn- mirror-blocks! [from to head]
  (store/transfer-closure! from to [head]))

(deftest a-follower-pins-a-publisher-and-then-tracks-it
  (with-stores
    (fn [publisher follower]
      (let [record (publish-definitions! publisher "demo" '[(defn double [x] (* x 2))] (seed 1))
            did (ed/did-key-from-seed (seed 1))]
        (is (= did (:publisher record)))
        (is (= 0 (:sequence record)))
        (mirror-blocks! publisher follower (:head record))
        (let [accepted (publication/accept-head! follower (:record record) {:publisher did})]
          (is (true? (:accepted? accepted)))
          (is (= (:head record) (store/head follower "demo"))))
        (testing "a second publication advances the follower without naming the key again"
          (authoring/update-namespace! publisher "demo" '[(defn double [x] (* x 3))])
          (let [next (publication/publish! publisher "demo" (seed 1))]
            (is (= 1 (:sequence next)))
            (mirror-blocks! publisher follower (:head next))
            (publication/accept-head! follower (:record next))
            (is (= (:head next) (store/head follower "demo")))))))))

(deftest a-first-follow-must-name-the-key-it-is-pinning
  (with-stores
    (fn [publisher follower]
      (let [record (publish-definitions! publisher "demo" '[(defn f [x] x)] (seed 1))]
        (mirror-blocks! publisher follower (:head record))
        (is (= :publication/publisher-required
               (:problem (ex-data (try (publication/accept-head! follower (:record record))
                                       (catch clojure.lang.ExceptionInfo e e))))))))))

(deftest a-different-key-cannot-take-over-a-followed-namespace
  (with-stores
    (fn [publisher follower]
      (let [record (publish-definitions! publisher "demo" '[(defn f [x] x)] (seed 1))
            did (ed/did-key-from-seed (seed 1))]
        (mirror-blocks! publisher follower (:head record))
        (publication/accept-head! follower (:record record) {:publisher did})
        ;; A second, perfectly valid signature -- by the wrong key.
        (authoring/update-namespace! publisher "demo" '[(defn f [x] (* x 99))])
        (let [imposter (publication/publish! publisher "demo" (seed 2))]
          (mirror-blocks! publisher follower (:head imposter))
          (is (= :publication/publisher-mismatch
                 (:problem (ex-data (try (publication/accept-head! follower (:record imposter))
                                         (catch clojure.lang.ExceptionInfo e e)))))))))))

(deftest a-tampered-record-does-not-verify
  (with-stores
    (fn [publisher follower]
      (let [record (publish-definitions! publisher "demo" '[(defn f [x] x)] (seed 1))
            did (ed/did-key-from-seed (seed 1))
            tampered (assoc (:record record) "namespace" "other")]
        (mirror-blocks! publisher follower (:head record))
        (is (= :publication/signature-invalid
               (:problem (ex-data (try (publication/accept-head! follower tampered
                                                                 {:publisher did})
                                       (catch clojure.lang.ExceptionInfo e e))))))))))

(deftest a-replayed-record-cannot-roll-a-follower-back
  (with-stores
    (fn [publisher follower]
      (let [first-record (publish-definitions! publisher "demo" '[(defn f [x] x)] (seed 1))
            did (ed/did-key-from-seed (seed 1))]
        (mirror-blocks! publisher follower (:head first-record))
        (publication/accept-head! follower (:record first-record) {:publisher did})
        (authoring/update-namespace! publisher "demo" '[(defn f [x] (* x 2))])
        (let [second-record (publication/publish! publisher "demo" (seed 1))]
          (mirror-blocks! publisher follower (:head second-record))
          (publication/accept-head! follower (:record second-record))
          (testing "re-serving the older, genuinely signed record is refused"
            (is (= :publication/sequence-not-advanced
                   (:problem (ex-data (try (publication/accept-head!
                                            follower (:record first-record))
                                           (catch clojure.lang.ExceptionInfo e e))))))))))))

(deftest a-record-whose-commit-is-absent-is-not-accepted
  (with-stores
    (fn [publisher follower]
      (let [record (publish-definitions! publisher "demo" '[(defn f [x] x)] (seed 1))
            did (ed/did-key-from-seed (seed 1))]
        ;; Blocks deliberately NOT mirrored: the signature is valid and the
        ;; follower still must not point at bytes it does not have.
        (is (= :codebase/block-not-found
               (:problem (ex-data (try (publication/accept-head! follower (:record record)
                                                                 {:publisher did})
                                       (catch clojure.lang.ExceptionInfo e e))))))
        (is (nil? (store/head follower "demo")))))))

(deftest a-record-that-skips-the-chain-is-refused
  (with-stores
    (fn [publisher follower]
      (let [first-record (publish-definitions! publisher "demo" '[(defn f [x] x)] (seed 1))
            did (ed/did-key-from-seed (seed 1))]
        (mirror-blocks! publisher follower (:head first-record))
        (publication/accept-head! follower (:record first-record) {:publisher did})
        (authoring/update-namespace! publisher "demo" '[(defn f [x] (* x 2))])
        (publication/publish! publisher "demo" (seed 1))
        (authoring/update-namespace! publisher "demo" '[(defn f [x] (* x 4))])
        (let [third (publication/publish! publisher "demo" (seed 1))]
          (mirror-blocks! publisher follower (:head third))
          (testing "sequence 2 does not link the record this follower actually has"
            (is (= :publication/broken-record-chain
                   (:problem (ex-data (try (publication/accept-head! follower (:record third))
                                           (catch clojure.lang.ExceptionInfo e e))))))))))))

(deftest retiring-a-follow-requires-naming-a-publisher-again
  (with-stores
    (fn [publisher follower]
      (let [record (publish-definitions! publisher "demo" '[(defn f [x] x)] (seed 1))
            did (ed/did-key-from-seed (seed 1))]
        (mirror-blocks! publisher follower (:head record))
        (publication/accept-head! follower (:record record) {:publisher did})
        (is (false? (:following? (publication/retire! follower "demo"))))
        (is (nil? (publication/following follower "demo")))
        (authoring/update-namespace! publisher "demo" '[(defn f [x] (* x 2))])
        (let [next (publication/publish! publisher "demo" (seed 1))]
          (mirror-blocks! publisher follower (:head next))
          (is (= :publication/publisher-required
                 (:problem (ex-data (try (publication/accept-head! follower (:record next))
                                         (catch clojure.lang.ExceptionInfo e e)))))))))))

(deftest the-publisher-did-is-derived-from-the-key-not-claimed
  (let [record (publication/sign-record (seed 3) {:namespace "demo"
                                                  :head "bafyreiaaaa" :sequence 0
                                                  :publisher "did:key:zSomethingElse"})]
    (is (= (ed/did-key-from-seed (seed 3)) (get record "publisher")))))
