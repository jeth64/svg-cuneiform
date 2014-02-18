(ns svg-cuneiform.core
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.zip :as zip]
            [svg-cuneiform.file :refer [get-svg get-translations get-paths get-lines
                                        update-and-save update-file]]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.contrib.seq :refer [positions]]
            [incanter
             [stats :refer :all]
             [core :refer :all]
             [charts :refer :all]
             [datasets :refer :all]]))


(def layer-id "cuneiforms")
(def file (get-svg "test/svg_cuneiform/images/3-0.svg"))
(def translations (get-translations file layer-id))
(def paths (get-paths file layer-id translations))
(def lines (get-lines file layer-id translations))
(def outfile "test/svg_cuneiform/images/out.svg")
(def delete ["path8055"])
(def new-paths []) ;;[[(k-means curve 4)]]
; file 3-0: "path2536" "path2540" "path1532"

;;
;; helper functions
;;

(defn- euclidean-squared [p1 p2]
  (reduce #(+ %1 (* %2 %2)) 0 (map - p1 p2)))

(defn- average [ptlist]
  (if (< (count ptlist) 1) nil
      (map #(/ % (count ptlist)) (apply map + ptlist))))

(defn- intersection
  "Returns intersection of lines through p1 and p2 and p3 and p4 respectively.
   Returns average of points if lines are parallel"
  [ptlist]
  (let [[p1 p2 p3 p4] ptlist
        [a1r a2] [(reverse (map - p1 p2)) (map - p3 p4)]
        b (map - p3 p1)
        determinant (apply - (map * a1r a2))
        l (print ptlist)
        avg (average ptlist)]
    (if (> (Math/abs determinant) 0.000001)
      (let [intersect (map #(- %1 (* %2 (/ (apply - (map * a1r b)) determinant))) p3 a2)
            pathlength (reduce + (map euclidean-squared ptlist (rest ptlist)) )]
        (if (< (euclidean-squared intersect avg) pathlength)
          intersect avg))
      avg)))


(defn k-means
  [data]
  (letfn [(closest-ind [pt ptlist]
            (apply min-key #(euclidean-squared pt (nth ptlist %))
                   (range (count ptlist))))
          (last-pt [ptlist]
            (apply max-key #(euclidean-squared (first ptlist) %)
                   ptlist))
          (nth-centroid [labels n]
            (average (map second (filter #(= n (first %)) (map vector labels data)))))
          ]
    (loop [old-labels (take (count data) (repeat -1))
           centroids (conj (vec (take 3 data)) (last-pt data))]
      (let [new-labels (map #(closest-ind % centroids) data)]
        (if (not= old-labels new-labels)
          (recur new-labels [(first centroids) (nth-centroid new-labels 1)
                             (nth-centroid new-labels 2) (last centroids)])
          centroids)))))


(defn k-means-reduce [ptlist]
  (letfn [(order-points [points]
            (apply min-key #(reduce + (map euclidean-squared % (rest %)))
                   (permutations points)))]
    (if (> (count ptlist) 4) (order-points (k-means ptlist)) ptlist)))


(def curve (second (first paths)))
(def labels (flatten [(take 6 (repeat 1)) (take 7 (repeat 2))])) ;13


(def reduced-paths (reduce #(update-in %1 [%2] k-means-reduce) paths (keys paths)))
(def references (reduce #(update-in %1 [%2] intersection) paths (keys paths)))



(defn main []
  (let [layer-id "cuneiforms";"g20"
        file (get-svg "test/svg_cuneiform/images/3-1.svg")
        ;file (get-svg "test/svg_cuneiform/images/VAT_10833-SeiteB_HPSchaudig.svg")
        outfile "test/svg_cuneiform/images/out.svg"
        translations (get-translations file layer-id)
        paths (get-paths file layer-id translations)
        lines (get-lines file layer-id translations)
        reduced-paths (reduce #(update-in %1 [%2] k-means-reduce) paths (keys paths))
        references (reduce #(update-in %1 [%2] intersection) reduced-paths (keys paths))

        ;new-paths (map #(vector (second %)) reduced-paths)
        ;new-paths (apply merge (map #(vector (second %)) reduced-paths) (map #(vector (repeat 4 (second %))) references))
        new-paths (map #(vector (cons [0 0] (repeat 3 (second %)))) references)
        to-delete [];[(keys paths)]
        ]
    (update-and-save file layer-id new-paths to-delete [] outfile)
    ))

(time (main))



;(defn regression-reduce [ptlist] ptlist)

;(def data (to-matrix (get-dataset :filip)))
;(def y (sel data :cols 0))
;(def x (sweep (sel data :cols 1)))

(def pts (get (get-paths file layer-id translations) "path2536"))
(def y (map first pts))
(def x (map second pts))

(def plot (scatter-plot x y))
(view plot)



;; the following line of code creates a matrix of the polynomial terms x, x^2, x^3
(def X (reduce bind-columns (for [i (range 1 4)] (pow x i))))

;; run the regression
(def lm (linear-model y X))
(print lm)
(:coefs lm)
(add-points plot x (map (partial + (last (:residuals lm))) (:fitted lm)))

(view plot)

(print pts)
;(def furthest-from-first [pts] (apply max-key #(euclidean-squared (first pts) %) (rest pts)))
