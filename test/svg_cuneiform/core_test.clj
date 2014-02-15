(ns svg-cuneiform.core-test
  (:require [clojure.test :refer :all]
            [svg-cuneiform.core :refer [reduce-path]]
            [clojure.core.matrix :as mat]))

(def eps 12.0)
(def approx-path-out [[385.518 460.314] [386.707 462.568] [388.285 463.561] [390.865,463.592]])
(def path-in [[385.75306 460.32645] [386.88106 462.36645000000004] [388.37306 463.31245] [390.70406 463.34045000000003] [391.02606000000003 463.34445] [391.02606000000003 463.84445] [390.70406 463.84045000000003] [388.19706 463.80945] [386.53306 462.77045000000004] [385.32106 460.57845000000003] [385.16506 460.29645000000005] [385.59706 460.04445000000004] [385.75306 460.32645]])

(deftest test-reduce-path
  (let [result (reduce-path path-in)]
    (is (or (< (mat/length-squared (map mat/sub approx-path-out result)) eps)
            (< (mat/length-squared (map mat/sub (reverse approx-path-out) result)) eps)))))


;(run-tests 'svg-cuneiform.core-test)
