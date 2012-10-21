(ns organum.core-test
  (:use clojure.test
        organum.core)
  (:require [clojure.java.io :as io]))

(deftest test-headline
  (testing "Parsing headline"
    (is (= (parse-headline "** TODO Some text :atag:btag:")
           (section 2 "Some text" ["atag" "btag"] "TODO")))))

(deftest test-block
  (testing "Parsing block heeader"
    (is (= (parse-block "  #+BEGIN_WIBBLE minor")
           (block "WIBBLE" "minor")))))

(deftest test-testfile
  (testing "Parsing test.org"
    (is (vector? (parse-file (io/resource "test.org"))))))