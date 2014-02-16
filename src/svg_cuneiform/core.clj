(ns svg-cuneiform.core
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.zip :as zip]
            [svg-cuneiform.file :refer [get-svg get-translations get-paths
                                        update-and-save update-file]]
            [clojure.core.matrix :as mat]
            [clojure.math.combinatorics :refer [permutations]]))


(def layer-id "cuneiforms")
(def file (get-svg "test/svg_cuneiform/images/3-0.svg"))
(def translations (get-translations file layer-id))
(def paths (get-paths file layer-id translations))
(def outfile "test/svg_cuneiform/images/out.svg")
(def delete ["path8055"])
(def new-paths []) ;;[[(second (k-means curve 4))]]

(defn k-means [data k]
  (letfn [(k-rand-nth [data k]
            (loop [rand-sel (take k (repeatedly #(rand-nth data)))]
              (if (= (count (set rand-sel)) k)
                rand-sel
                (recur (take k (repeatedly #(rand-nth data)))))))
          (closest-ind [pt ptlist]
            (apply min-key #(mat/length-squared (mat/sub pt (nth ptlist %)))
                   (range (count ptlist))))
          (average [recs] (map #(/ % (count recs)) (apply mat/add recs)))
          (update-centroids [data labels]
            (map average (persistent!
                          (reduce #(assoc! %1 (nth labels %2)
                                           (cons (nth data %2) (get %1 (nth labels %2))))
                                  (transient (vec (take (count (set labels)) (repeat []))))
                                  (range (count data))))))]
      (loop [old-labels (take (count data) (repeat -1))
             centroids (k-rand-nth data k)]
        (let [new-labels (map #(closest-ind % centroids) data)]
          (if (= old-labels new-labels)
            [new-labels centroids]
            (recur new-labels (update-centroids data new-labels)))))))

(def curve (second (first (get-paths file layer-id translations))))

(defn reduce-path [ptlist]
  (letfn [(euclidean-squared [a b] (mat/length-squared (mat/sub a b)))]
    (let [pts (second (k-means curve 4))]
      (apply min-key #(reduce + (map euclidean-squared % (rest %)))
             (permutations pts)))))


(reduce-path curve)


(update-and-save file layer-id [[(reduce-path curve)]] delete outfile)
