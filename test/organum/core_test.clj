(ns organum.core-test
  (:use clojure.test
        organum.core)
  (:require [clojure.java.io :as io])
  (:import [organum.core Root Section Block Drawer Line]))

(deftest headline
  (testing "Parsing headline"
    (is (= (parse-headline "** TODO Some text :atag:btag:")
           (Section. 2 "Some text" nil ["atag" "btag"] "TODO")))))

(deftest block
  (testing "Parsing block heeader"
    (is (= (parse-block "  #+BEGIN_WIBBLE minor")
           (Block. "WIBBLE" "minor" nil)))))

(deftest testfile
  (testing "Parsing test.org"
    (is (vector? (parse-file (io/resource "test.org"))))))