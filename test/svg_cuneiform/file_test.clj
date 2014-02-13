(ns svg-cuneiform.file-test
  (:require [clojure.test :refer :all]
            [svg-cuneiform.file :refer [get-svg get-paths get-transformations
                                        pathid-points-map clean-file update-file]]
            [clojure.zip :as zip]))

(def layer-id "cuneiforms")

(defn file [nr] (get-svg (str "test/svg_cuneiform/images/" nr ".svg")))


(deftest test-get-paths-3-0
  (let [path-map (get-paths (file "3-0") layer-id)]
    (is (= (first (get path-map "path2540")) '(0 0) ))
    (is (= (count path-map) 4))))


(deftest test-get-transformations-3-0
  (let [transforms (get-transformations (file "3-0") layer-id)]
    (is (= (first transforms) ['("path2532") '(["translate" (385.72286 466.37185)])]))
    (is (= (count transforms) 4))))

(deftest test-get-transformations-1-0
  (is (= (get-transformations (file "1-0") layer-id) '())))


(deftest test-path-points-map-3-0
  (let [paths (pathid-points-map (file "3-0") layer-id)]
    (is (= (first (get paths "path2540")) '(385.75306 460.32645))) ))

(deftest test-path-points-map-1-0
  (let [paths (pathid-points-map (file "1-0") layer-id)]
    (is (= (last (get paths "path8045")) '(216.42 531.635)))
    (is (= (count paths) 6))))


(deftest test-clean-file
  (let [clean (clean-file (file "1-0") '("path8037" "path8045" "path8053"))]
    (is (= (count (last clean)) 2))
    (is (= (count (last (file "1-0"))) 5))))


(deftest test-update-file-3-0
  (let [mat1342 [(filter #(= 4 (count %))
                         (vals (pathid-points-map (file "1-0") layer-id)))]
        wedges [["path8053" "path8037" "path8045"]]
        updated (last (update-file (file "1-0") layer-id mat1342 wedges))]
    (is (= (count updated) 3))
    (is (= (last updated) '(:path {:d "M205.086,515.051 C214.753,516.551 222.336,516.385 240.419,513.301 C227.086,515.802 217.753,520.052 217.086,524.968 C218.92,519.968 217.003,522.718 216.42,531.635", :fill "none", :stroke "blue", :id "wedge0", :stroke-width 0.5})))))

;(def outfile "test/svg_cuneiform/images/out.svg")

;(run-tests 'svg-cuneiform.file-test)
