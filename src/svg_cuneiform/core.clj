(ns svg-cuneiform.core
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.zip :as zip]
            [svg-cuneiform.file :refer [get-svg get-translations get-paths get-lines
                                        update-and-save update-file]]
            [clojure.math.combinatorics :refer [permutations]]
            [incanter [stats :refer :all]]))


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

(defn end-slopes [p1 p2 p3 p4]
  (letfn [(whiten [v] (map #(/ % (euclidean-distance [0 0] v)) v))]
    (map whiten [(map - p1 p2) (map - p4 p3)])))


(def reduced-paths (reduce #(update-in %1 [%2] k-means-reduce) paths (keys paths)))
(def references (reduce #(update-in %1 [%2] intersection) reduced-paths (keys reduced-paths)))
(def slopes (reduce #(update-in %1 [%2] (partial apply end-slopes))
                    reduced-paths (keys reduced-paths)))


(print reduced-paths)
(print slopes)

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
        slopes (reduce #(update-in %1 [%2] (partial apply end-slopes)) reduced-paths (keys paths))

        ;; new-paths (map #(vector (second %)) reduced-paths)
        ;; show points:
        ;; new-paths (map #(vector (cons (map (partial + 0.5) (second %)) (repeat 3 (second %)))) references)
        ;; show slopes: not yet working
        ;; new-paths (merge [] (map #(vector (cons (map + (first %1) (first %2)) (repeat 3 (first %2)))) (vals slopes) (vals reduced-paths)) (map #(vector (cons (map + (last %1) (last %2)) (repeat 3 (last %2)))) (vals slopes) (vals reduced-paths)))

        paths-to-delete [];[(keys paths)]
        lines-to-delete []
        ]
   ;; (update-and-save file layer-id new-paths paths-to-delete lines-to-delete outfile)
    ;; new-paths

    ))

(time (main))
