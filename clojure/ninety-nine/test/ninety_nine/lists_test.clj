(ns ninety-nine.lists-test
  (:require [clojure.test :refer :all]
            [ninety-nine.lists :refer :all]))

(deftest test-my-last
  (testing "Test `my-last`"
    (are [x y] (= x (my-last y))
      nil []
      :x [:x]
      :y [:x :y]
      [:z] [:x :y [:z]]

      nil ()
      :x '(:x)
      :y '(:x :y)
      '(:z) '(:x :y (:z)))))

