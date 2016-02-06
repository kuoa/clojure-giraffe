(ns mastermind.core-test
  (:require [clojure.test :refer :all]
            [mastermind.core :refer :all]))

(deftest valid-move-1
  (testing "Wrong size"
   (is (= false (valid-move? 4 "ybrgc")))))

(deftest valid-move-2
  (testing "Wrong letters"
    (is (= false (valid-move? 4 "ybgz")))))

(deftest valid-move-3
  (testing "Valid move"
   (is (= true  (valid-move? 4 "rgby")))))

(deftest color-keyword
  (testing "Conversion to color keywords"
   (is (= [:red :green :blue :yellow]  (to-color-keyword "rgby")))))















