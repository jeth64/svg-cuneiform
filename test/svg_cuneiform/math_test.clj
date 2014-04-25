(ns svg-cuneiform.math-test
  (:require [clojure.test :refer :all]
            [svg-cuneiform.math :refer :all]))


(deftest test-cubic-zeros
  (testing "D>0"
    (let [roots (cubic-zeros 1 0 6 -20)]
      (is (= 1 (count roots)))
      (is (= 2 (first (round 0 (first roots)))))))
  (testing "D<0"
    (let [roots (cubic-zeros 1 -3 -144 432)]
      (is (= 3 (count roots)))
      (is (= '(-12 3 12) (sort (map (partial round 0) roots)))))))


(map (partial polynomial [1 -3 -144 432])
     (cubic-zeros 1 -3 -144 432)) ;; sollte [0 0 0] sein


(bezier-coeffs [1 2 3 4])
