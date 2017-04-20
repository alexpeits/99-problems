(ns ninety-nine.lists-test
  (:require [clojure.test :refer :all]
            [ninety-nine.lists :refer :all]))

(deftest test-my-last
  (testing "Testing `my-last`"
    (is (= nil (my-last [])))
    (is (= 2 (my-last [1 2])))))
