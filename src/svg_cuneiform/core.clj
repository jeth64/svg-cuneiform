(ns svg-cuneiform.core
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.zip :as zip]
            [svg-cuneiform.file :refer [get-svg get-translations get-paths get-lines
                                        update-and-save update-file]]
            [clojure.math.combinatorics :refer [permutations]]))


(def layer-id "cuneiforms")
(def file (get-svg "test/svg_cuneiform/images/3-0.svg"))
(def translations (get-translations file layer-id))
(def paths (get-paths file layer-id translations))
(def reduced-paths (reduce #(update-in %1 [%2] reduce-path) paths (keys paths)))
(def lines (get-lines file layer-id translations))
(def outfile "test/svg_cuneiform/images/out.svg")
(def delete ["path8055"])
(def new-paths []) ;;[[(second (k-means curve 4))]]


(let [paths (get-paths file layer-id translations)]
  (reduce #(update-in %1 [%2] reduce-path) paths (keys paths))
  )

(defn- euclidean-squared [p1 p2]
  (reduce #(+ %1 (* %2 %2)) 0 (map - p1 p2)))

(defn- average [ptlist]
  (if (< (count ptlist) 1) nil
      (map #(/ % (count ptlist)) (apply map + ptlist))))


(defn k-means
  "Performs k-means clustering. Returns empty vector after 5 restarts."
  [data k]
  (letfn [(k-rand-nth [data k]
            (loop [rand-sel (take k (repeatedly #(rand-nth data)))]
              (if (= (count (set rand-sel)) k)
                rand-sel
                (recur (take k (repeatedly #(rand-nth data)))))))
          (closest-ind [pt ptlist]
            (apply min-key #(euclidean-squared pt (nth ptlist %))
                   (range (count ptlist))))
          (update-centroids [data labels]
            (map average (persistent!
                          (reduce #(assoc! %1 (nth labels %2)
                                           (cons (nth data %2) (get %1 (nth labels %2))))
                                  (transient (vec (take (count (set labels)) (repeat []))))
                                  (range (count data))))))]
      (loop [old-labels (take (count data) (repeat -1))
             centroids (k-rand-nth data k)
             i 0]
        (let [new-labels (map #(closest-ind % centroids) data)]
          (if (not= old-labels new-labels)
            (let [new-centroids (filter identity (update-centroids data new-labels))]
              (if (not= (count new-centroids) 4)
                (if (> i 5) []
                  (recur (take (count data) (repeat -1)) (k-rand-nth data k) (inc i)))
                (recur new-labels new-centroids i)))
            [new-labels centroids])))))


(defn reduce-path [ptlist]
  (letfn [(order-points [points]
            (apply min-key #(reduce + (map euclidean-squared % (rest %)))
                   (permutations points)))]
    (if (> (count ptlist) 4) (order-points (second (k-means ptlist 4))) ptlist)))


(second (first reduced-paths))

(defn main []
  (let [layer-id "cuneiforms"
        file (get-svg "test/svg_cuneiform/images/3-1.svg")
        outfile "test/svg_cuneiform/images/out.svg"
        translations (get-translations file layer-id)
        paths (get-paths file layer-id translations)
        lines (get-lines file layer-id translations)
        reduced (reduce #(update-in %1 [%2] reduce-path) paths (keys paths))

        new-paths []
        to-delete [(keys paths)]
        ]
    (update-and-save file layer-id (map #(vector (second %)) reduced) to-delete outfile)

    ))

(time (main))
