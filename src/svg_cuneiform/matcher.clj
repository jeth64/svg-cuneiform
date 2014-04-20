(ns svg-cuneiform.matcher
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.zip :as zip]
            [clojure.set :refer [union]]
            [clojure.math.combinatorics :refer [permutations selections combinations]]
            [incanter [core :refer [matrix decomp-svd solve-quadratic]]]
            [svg-cuneiform.math :refer :all]
            ))

;;
;; Curve formatting
;;


(defn- k-means
  "Modified 4-means with 2 fixed centroids"
  [data]
  (letfn [(closest-ind [pt ptlist]
            (apply min-key #(euclidean-squared pt (nth ptlist %))
                   (range (count ptlist))))
          (nth-centroid [labels n]
            (average (map second (filter #(= n (first %))
                                         (map vector labels data)))))
          ]
    (loop [old-labels (take (count data) (repeat -1))
           centroids (conj (vec (take 3 data)) (furthest-from-first data))]
      (let [new-labels (map #(closest-ind % centroids) data)]
        (if (not= old-labels new-labels)
          (recur new-labels [(first centroids) (nth-centroid new-labels 1)
                             (nth-centroid new-labels 2) (last centroids)])
          centroids)))))


(defn- k-means-reduce [ptlist]
  (letfn [(order-points [points]
            (apply min-key #(reduce + (map euclidean-squared % (rest %)))
                   (permutations points)))]
    (if (> (count ptlist) 4) (order-points (k-means ptlist)) ptlist)))


(defn classify-paths ;; TODO: improve; threshold varies
  "Uses second singular value of data lists to distiguish lines and curves.
   Returns [curve-map line-map]"
  [paths threshold]
  ;; (map (partial apply mapv vector))
  (vals (apply sorted-set (group-by (comp (partial > threshold) second :S
                                          decomp-svd matrix whiten second) paths))))


(defn classify-and-reduce [paths threshold]
  (let [[curves lines] (classify-paths paths threshold)]
    [(zipmap (keys curves) (map k-means-reduce (vals curves)))
     (zipmap (keys lines) (map #(vector (first %) (furthest-from-first %)) (vals lines)))]))


;;
;; Matcher functionality
;;


(defn reference
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


(defn- get-triples [ptlist]
  (letfn [;; returns for each point indices of three closest points
          ;; including itself
          (three-closest-pts [ptlist]
            (map keys
                 (map #(take 3 (sort-by last
                                        (zipmap (range (count ptlist)) %)))
                           (pairwise-dist ptlist ptlist))))]
    (map (comp vec first)
         (filter #(= 3 (second %))
                 (frequencies (map set (three-closest-pts ptlist)))))))


(defn- flip-curves [reduced-paths triple]
  (letfn [(costs [possibility]
            (reduce + (map #(euclidean-squared (last %1) (first %2))
                           possibility
                           (next (cycle possibility)))))]
    (let [curves (replace reduced-paths triple)
          flips (selections [true false] 3)
          possibilities (map (partial map #(if %3 %1 %2)
                                      curves (map reverse curves)) flips)]
      (apply min-key costs possibilities))))


(defn- merge-ends [curves refpoints]
  (let [midpoint (average refpoints)]
    (reduce #(if (< (euclidean-squared midpoint (last (nth curves %2)))
                    (euclidean-squared midpoint (first (nth curves (mod (inc %2) 3)))))
               (assoc %1 %2
                      (conj (vec (drop-last (nth %1 %2)))
                            (first (nth %1 (mod (inc %2) 3)))))
               (assoc %1 (mod (inc %2) 3)
                      (cons (last (nth %1 %2))
                            (rest (nth %1 (mod (inc %2) 3))))))
            (vec curves) (range 3))))


(defn check-proximity [curves]
  (let [lines (map #(take-nth 3 %) curves)
        isec-lend-dists (map #(vector (apply min (map (partial euclidean-squared (first %2))
                                                      (curve-line-intersection %1 %2)))
                                      (apply min (map (partial euclidean-squared (last %3))
                                                      (curve-line-intersection %1 %3))))
                             curves (next (cycle lines)) (next (next (cycle lines))))
        [dists1 dists2] (transpose isec-lend-dists)]
    (every? (partial > 1) (map min (next (cycle dists2)) dists1))))


(defn- valid? [curves]
  (letfn [(get-slopes [p1 p2 p3 p4]
            (map normalize [(map - p1 p2) (map - p4 p3)]))]
    (let [slopes (map (partial apply get-slopes) curves)
          cos-alphas (map #(dot-product (last %1) (first %2))
                          slopes (next (cycle slopes))) ;; alpha: angle between slopes of curves
          ends (apply concat (map (partial take-nth 3) curves))
          b (map (partial apply euclidean-squared)
                 (combinations (map #(normalize (map - (average ends) %)) ends) 2))
          num-sim-slopes (count (filter (partial > 0.2) b))

        ;;  a (if c (print b "=> num-sim-slopes: " num-sim-slopes "proximity: " c) nil)
          ]
      (and ;;(some #(< 0 % 1) cos-alphas)
          ;; (= 3 num-sim-slopes)
           (check-proximity curves)
           ))))


(defn first-strategy-rec [curve-map]
  (let [curve-enums (vec (keys curve-map))
        curves (vec (vals curve-map))
        references (mapv reference curves)

        triples (get-triples references)
        wedges (filter #(valid? (second %))
                       (zipmap triples
                               (map (partial flip-curves curves)
                                    triples)))
        max-dist (reduce max 0
                         (flatten (map #((fn [l] (map euclidean-squared l (cycle (rest l))))
                                         (replace references (first %))) wedges)))
        used-keys (flatten (keys wedges))
        paths (map #(merge-ends (second %) (replace references (first %)))
                   wedges)]
    [paths (set (replace curve-enums used-keys)) max-dist]))

(defn first-strategy
  "Find 3 closest curves from each curve including itself.
   If there are 3 identical such triples, check if they can be curves
   and merge them into 1 polybezier"
  [curve-map]
  (loop [curves curve-map wedges [] used-keys #{} max-dist 0]
    (let [[w keys new-max] (first-strategy-rec curves)]
      (if (= 0 (count keys))
        [wedges used-keys max-dist curves]
        (recur (filter #(not (contains? keys (key %))) curves)
               (concat wedges w)
               (union used-keys keys) ;; TODO: nicht in rek sondern vgl keys curve-map curves?
               (max new-max max-dist))))))

(defn- get-triples2 [dist-matrix max-dist]
  (letfn [(closest-inds [n dists]
            (sort (keys (filter (fn [[k v]] (< v max-dist))
                                (sort-by last (zipmap (range n) dists))))))]
    (set (apply concat (map (comp (fn [l] (combinations l 3))
                                  (partial closest-inds (count (first dist-matrix))))
                            dist-matrix)))))

(defn second-strategy [curve-map max-dist]
  (let [curve-enums (vec (keys curve-map))
        curves (vec (vals curve-map))
        references (mapv reference curves)
        triples (get-triples2 (pairwise-dist references references) max-dist)
        wedges (filter #(valid? (second %))
                       (zipmap triples
                               (map (partial flip-curves curves) triples)))
        used-keys (flatten (keys wedges))
        paths (map #(merge-ends (second %) (replace references (first %)))
                   wedges)]
    [paths (set (replace curve-enums used-keys))]))


(defn find-wedges [curve-map]
  (let [[wedges used-keys max-dist rest-curves] (first-strategy curve-map)
        [wedges2 keys2] (second-strategy rest-curves max-dist)]
    [(concat wedges2 wedges) (union used-keys keys2)]
    ))


(defn add-extension
  "Returns modified wedges and 2 lists: path keys and line keys of lines used"
  [wedges line-map max-dist arccos-allowed-angle]
  (let [corners (mapv (partial mapv first) wedges)
        corner-dir-vecs (apply concat (map (comp (partial map normalize)
                                                 (partial take-nth 4)
                                                 whiten
                                                 (partial apply concat)) wedges));; from center to corner
        c-corners (apply concat corners)
        c-lines (vec (apply concat (vals line-map)))
        line-dirs (mapv #(normalize (map - (second %) (first %))) (vals line-map))
        line-dir-vecs (vec (interleave line-dirs (mapv (partial mapv unchecked-negate) line-dirs)))
        poss-ext (mapv #(vec (keys (sort-by last (filter (comp (partial > max-dist) second)
                                                        (zipmap (range (count %)) %)))))
                      (pairwise-dist c-corners c-lines))
        [used-keys new-corners] (apply mapv vector
                                       (mapv #(loop [i 0]
                                               (if (< i (count %3))
                                                 (if (< arccos-allowed-angle
                                                        (dot-product %2 (get line-dir-vecs (get %3 i))))
                                                   (let [ind (get %3 i)]
                                                      [(get (vec (keys line-map)) (quot ind 2))
                                                       (get c-lines (if (even? ind) (inc ind) (dec ind)))])
                                                    (recur (inc i)))
                                                  [nil %1]))
                                             c-corners corner-dir-vecs poss-ext))
        ]
    [(mapv #(mapv concat %2 %1 (next (cycle %2)))
          (mapv (partial mapv (comp butlast rest)) wedges)
          (mapv (partial mapv vector) (partition 3 new-corners)))
     (set (filter identity used-keys))]))
