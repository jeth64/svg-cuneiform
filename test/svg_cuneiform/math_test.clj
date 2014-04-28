(ns svg-cuneiform.math-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer [facts fact]]
            [svg-cuneiform.math :refer :all]))


(facts "About 'round'"
       (fact "integer rounding succeeds"
             (round 0 2.5) => 3)
       (fact "decimal rounding succeeds"
             (round 1 2.05) => 2.1))

(facts "About 'cubic-zeros'"
       (fact "works for an example with D>0"
             (map int (cubic-zeros 1 0 6 -20))
             => [2])
       (fact "works for an example with D<0"
             (sort (map round (cubic-zeros 1 -3 -144 432)))
             => [-12 3 12])
       (fact "works for random integer coefficients"
             (take 5 (repeatedly
                      #(let [coeffs (take 4 (repeatedly (fn [] (rand-int 1000))))
                            res (map (comp round (partial polynomial coeffs))
                                     (apply cubic-zeros coeffs))]
                        (if (every? zero? res)
                          true
                          (str "not true for " (vec coeffs))))))
             => (partial every? true?)))
