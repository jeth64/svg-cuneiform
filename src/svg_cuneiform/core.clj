(ns svg-cuneiform.core
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.zip :as zip]
            [svg-cuneiform.file :refer
             [get-svg get-translations get-paths get-lines
              update-and-save update-file]]
            [clojure.math.combinatorics :refer [permutations selections]]
            [incanter [stats :refer :all] [core :refer [matrix decomp-svd]]]
            ))



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

(defn cross-product [a b];; = determinant
  (- (* (first a) (second b)) (* (first b) (second a))))

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
  "Modified 4-means with 2 fixed centroids"
  [data]
  (letfn [(closest-ind [pt ptlist]
            (apply min-key #(euclidean-squared pt (nth ptlist %))
                   (range (count ptlist))))
          (last-pt [ptlist]
            (apply max-key #(euclidean-squared (first ptlist) %)
                   ;;(take-nth 3)
                   ptlist))
          (nth-centroid [labels n]
            (average (map second (filter #(= n (first %))
                                         (map vector labels data)))))
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


(defn get-triples [ptlist]
  (letfn [;; returns simple matrix
          (pairwise-dist [ptlist]
            (map #(map (partial euclidean-squared %) ptlist) ptlist))
          ;; returns for each point indices of three closest points
          ;; including itself
          (three-closest-pts [ptlist]
            (map keys
                 (map #(take 3 (sort-by last
                                        (zipmap (range (count ptlist)) %)))
                           (pairwise-dist ptlist))))]
    (map (comp vec first)
         (filter #(= 3 (second %))
                 (frequencies (map set (three-closest-pts ptlist)))))))

(defn flip-curves [reduced-paths triple]
  (letfn [(costs [possibility]
            (reduce + (map #(euclidean-squared (last %1) (first %2))
                           possibility
                           (next (cycle possibility)))))]
    (let [curves (replace reduced-paths triple)
          flips (selections [true false] 3)
          possibilities (map (partial map #(if %3 %1 %2)
                                      curves (map reverse curves)) flips)]
      (apply min-key costs possibilities))))

(defn merge-ends [curves refpoints]
  (let [midpoint (average refpoints)]
    (reduce #(if (< (euclidean-squared midpoint (last (nth curves %2)))
                    (euclidean-squared
                     midpoint (first (nth curves (mod (inc %2) 3)))))
               (assoc %1 %2
                      (conj (vec (drop-last (nth %1 %2)))
                            (first (nth %1 (mod (inc %2) 3)))))
               (assoc %1 (mod (inc %2) 3)
                      (cons (last (nth %1 %2))
                            (rest (nth %1 (mod (inc %2) 3))))))
            (vec curves) (range 3))))

(defn valid? [curves]
  (letfn [(whiten [v] (map #(/ % (euclidean-distance [0 0] v)) v))
          (end-slopes [p1 p2 p3 p4]
            (map whiten [(map - p1 p2) (map - p4 p3)]))
          (dot-product [a b] (reduce + (map * a b)) )]
    (let [slopes (map (partial apply end-slopes) curves)
          cos-alphas (map #(dot-product (last %1) (first %2))
                         slopes (next (cycle slopes)))]
      (some #(< 0 % 1) cos-alphas))))

(defn get-wedges [reduced-paths references]
  (let [triples (get-triples references)
        wedges (filter #(valid? (second %))
                       (zipmap triples
                               (map (partial flip-curves reduced-paths)
                                    triples)))
        used-keys (flatten (keys wedges))
        paths (map #(merge-ends (second %) (replace references (first %)))
                   wedges)]
    [paths used-keys]))


(def enumerations (vec (keys paths)))
(def reduced-paths (vec (map k-means-reduce (vals paths))))
(def references (vec (map intersection reduced-paths)))

(def ptlist references)
(print enumerations)
(print reduced-paths)
(print references)

(defn pt-line-dist [p]
  (reduce #(+ %1 (/ (Math/abs (cross-product (map - (first p) (last p))
                                             (map - %2 (first p))))
                    (Math/sqrt (euclidean-squared (map - (first p) (last p))
                                                  [0 0]))))
       0 (rest (butlast p))))

(defn classify-paths-old [p threshold]
  (map (partial apply mapv vector)
       (vals (group-by #(> (pt-line-dist (second %)) threshold) p))))


(defn whiten [data]
  (map #(map - % (average data)) data))

(defn classify-paths [paths]; vor reduced
  (map (partial apply mapv vector)
       (vals (apply sorted-set (group-by (comp (partial > 1.0) second :S decomp-svd
                                               matrix whiten second) paths)))))


(time (main))
(defn main []
  (let [[layer-id filename]
        ["cuneiforms" "test/svg_cuneiform/images/3-1.svg"]
       ;; ["g20" "test/svg_cuneiform/images/VAT_10833-SeiteB_HPSchaudig.svg"]
        file (get-svg filename)
        outfile "test/svg_cuneiform/images/out.svg"
        translations (get-translations file layer-id)
        [curve-map line-map] (classify-paths (get-paths file layer-id translations))
        enumerations (vec (first curve-map))
        curves (vec (map k-means-reduce (second curve-map)))

        ;; lines (apply merge (get-lines file layer-id translations) line-map)
        references (vec (map intersection curves))
        [wedges used-keys] (get-wedges curves references)

        new-paths (concat wedges (map #(vector (cons (map (partial + 0.5) %) (repeat 3 %))) references))
        ;; show points:
        ;; new-paths (map #(vector (cons (map (partial + 0.5) %) (repeat 3 %))) references)
        ;; show slopes:
        ;; new-paths (concat (map #(vector (cons (map + (map (partial * -5) (first %1)) (first %2)) (repeat 3 (first %2)))) slopes reduced-paths) (map #(vector (cons (map + (map (partial * -5) (last %1)) (last %2)) (repeat 3 (last %2)))) slopes reduced-paths))

        paths-to-delete (replace enumerations used-keys);[(keys paths)]
        lines-to-delete []
        ]

 (update-and-save file layer-id new-paths paths-to-delete lines-to-delete outfile)
       ))
