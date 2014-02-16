(ns svg-cuneiform.file-test
  (:require [clojure.test :refer :all]
            [svg-cuneiform.file :refer [get-svg get-translations get-paths get-lines
                                        clean-file update-file update-and-save]]
            [clojure.zip :as zip]
            [clojure.java.io :as io]))

(def layer-id "cuneiforms")

(defn file [nr] (get-svg (str "test/svg_cuneiform/images/" nr ".svg")))
(def outfile "test/svg_cuneiform/images/test-out.svg")


(deftest test-get-translations-3-0
  (let [transforms (get-translations (file "3-0") layer-id)]
    (is (= (last transforms) ["path2532" '(385.72286 466.37185)]))
    (is (= (count transforms) 4))))

(deftest test-get-translations-1-0
  (is (= (get-translations (file "1-0") layer-id) '())))


(deftest test-get-paths-3-0
  (let [translations (get-translations (file "3-0") layer-id)
        paths (get-paths (file "3-0") layer-id translations)]
    (is (= (first (get paths "path2540")) '(385.75306 460.32645)))
    (is (= (count paths) 4))))

(deftest test-get-paths-1-0
  (let [translations (get-translations (file "1-0") layer-id)
        paths (get-paths (file "1-0") layer-id translations)]
    (is (= (last (get paths "path8045")) '(216.42 531.635)))
    (is (= (count paths) 6))))


(deftest test-get-lines-2-0
  (let [translations (get-translations (file "2-0") layer-id)
        lines (get-lines (file "2-0") layer-id translations)]
    (is (= (first (get lines "line14024")) '(305.167 270.891) ))
    (is (= (count lines) 1))))

(deftest test-get-lines-3-0
  (let [translations (get-translations (file "3-0") layer-id)
        lines (get-lines (file "3-0") layer-id translations)]
    (is (= lines {}))))


(deftest test-clean-file
  (let [clean (clean-file (file "1-0") '("path8037" "path8045" "path8053"))]
    (is (= (count (last clean)) 2))
    (is (= (count (last (file "1-0"))) 5))))

(deftest test-update-file-3-0
  (let [translations (get-translations (file "1-0") layer-id)
        mat1342 [(filter #(= 4 (count %))
                         (vals (get-paths (file "1-0") layer-id translations)))]
        wedges [["path8053" "path8037" "path8045"]]
        updated (last (update-file (file "1-0") layer-id mat1342 wedges))]
    (is (= (count updated) 3))
    (is (= (last updated) '(:path {:d "M205.086,515.051 C214.753,516.551 222.336,516.385 240.419,513.301 C218.92,519.968 217.003,522.718 216.42,531.635 C227.086,515.802 217.753,520.052 217.086,524.968", :fill "none", :stroke "blue", :id "wedge0", :stroke-width 0.5})))))

(deftest test-update-and-save
  (update-and-save (file "1-0") layer-id [] [] outfile)
  (is (.exists (io/as-file outfile)))
  (io/delete-file outfile true))


(run-tests 'svg-cuneiform.file-test)
