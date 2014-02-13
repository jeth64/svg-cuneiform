(ns svg-cuneiform.svg-test
  (:require [clojure.test :refer :all]
            [svg-cuneiform.svg :refer [parse-transform parse-path matrix2path]]))


(def res-single '(["translate" (543.654 544.65)]))
(def res-multi '(["translate" (543.654 544.65)] ["scale" (643 535)]))
(def single-transform "translate(543.654,544.65)")
(def multi-transform "translate(543.654,544.65) scale(643, 535)")

(def pt-list ['(100 200) '(100 100) '(250 100) '(250 200) '(250 300) '(400 300) '(400 200)])
(def C-path "M100,200 C100,100 250,100 250,200 C250,300 400,300 400,200")
(def c-path "M100,200 c0,-100 150,-100 150,0 c0,100 150,100 150,0")
(def CS-path "M100,200 C100,100 250,100 250,200 S400,300 400,200")
(def cs-path "M100,200 c0,-100 150,-100 150,0 s150,100 150,0")
;; parse-path might not work with Cc or Cs


(deftest test-parse-single-transform
  (let [transforms (parse-transform single-transform)]
    (is (= (count transforms) 1))
    (is (= (count (first transforms)) 2))
    (is (= transforms res-single))))

(deftest test-parse-multi-transform
  (let [transforms (parse-transform multi-transform)]
    (is (= (count transforms) 2))
    (is (= (count (second transforms)) 2))
    (is (= transforms res-multi))))


(deftest test-parse-path-with-C
  (is (= (parse-path C-path) pt-list)))

(deftest test-parse-path-with-c
  (is (= (parse-path c-path) pt-list)))

(deftest test-parse-path-with-S
  (is (= (parse-path CS-path) pt-list)))

(deftest test-parse-path-with-s
  (is (= (parse-path cs-path) pt-list)))


(deftest test-matrix2path-number-amount
  (is (= (* 2 (count pt-list))
         (count (re-seq  #"-?(?:\d*\.?)\d+" (matrix2path (partition 4 3 pt-list)))))))


(run-tests 'svg-cuneiform.svg-test)
