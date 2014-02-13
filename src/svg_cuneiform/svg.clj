(ns svg-cuneiform.svg
  (:require [clojure.string :refer [join split trim]]))

;; Unchecked parsing: invalid transform or path strings will cause unknown behaviour


;;
;; Helper functions
;;

(defn- get-char-ptlist-pairs [str]
     (map #(vector (second %)
                   (partition 2 (map (partial read-string)
                                     (re-seq #"-?(?:\d*\.?)\d+" (last %)))))
          (re-seq #"([mMcCsS])([^A-Za-z]+)" str)))

(defn- add-first-cntrl-pts [first offset points]
  [offset (cons first
          (butlast (apply concat
                          (map #(let [[one two] %]
                                  [one two (map - two one)])
                               (partition 2 points)))))])

(defn- make-absolute
  "Returns new offset and list of absolute points.
   Every n-th point in list the offset is updated for the following points:
   new offset = old offset + current relative point = current absolute point"
  [initial-offset len-kn-list n]
  (letfn [(pt-add [pt pt-list] (map (partial map + pt) pt-list))]
    (let [numbers (partition n len-kn-list)]
      (loop [offset initial-offset point-list (transient []) i 0]
        (if (< i (count numbers))
          (let [newpoints (pt-add offset (nth numbers i))]
            (recur (last newpoints) (reduce conj! point-list newpoints) (inc i)))
          [offset (persistent! point-list)])))))


;;
;; Functionality for retrieving transformations
;;

(defn parse-transform
  "Returns list of method-numbers pairs"
  [str]
  (if (string? str)
    (map #(let [[method argstr] (rest %)
                arglist (map read-string (split (trim argstr) #"\s*,\s*"))]
            [method arglist])
         (re-seq #"(\w+)\(([^)]*)\)" (trim str)))
    '() ))


;;
;; Functionality for extracting points from path string
;;

(defn parse-path
  "Returns list of absolute points (length: 1+3*x)."
  [polybezier-string]
  (let [cmd-numbers-list (get-char-ptlist-pairs polybezier-string)
        first-pt (first (second (first cmd-numbers-list)))]
    (loop [offset first-pt i 1 points [first-pt]]
      (if (< i (count cmd-numbers-list))
        (let [[cmd numbers] (nth cmd-numbers-list i)
              [offset new-pts] (case cmd
                                 "c" (make-absolute offset numbers 3)
                                 "C" [offset numbers]
                                 "s" (apply add-first-cntrl-pts
                                            (map + (last points)
                                                 (apply map - (reverse (take-last 2 points))))
                                            (make-absolute offset numbers 2))
                                 "S" (add-first-cntrl-pts
                                      (map + (last points)
                                           (apply map - (reverse (take-last 2 points))))
                                      offset numbers))]
          (recur offset (inc i) (reduce conj points new-pts) ))
        points))))

;(defn path2matrix [pathstring] (partition 4 3 (parse-path pathstring)))


;;
;; Functionality for producing a path-string
;;


(defn matrix2path
  "Return string for polybezier curve (input: nx4x2-matrix)"
  [matrix]
  (letfn [(matrix-round [mat]
            (map (partial map (partial map #(float (/ (Math/round (* (float %) 1000)) 1000))))
                 mat))]
    (let [mat (matrix-round matrix)
          M (str "M" (join "," (first (first mat))))
          C (join " " (map #(str "C" (join " " (map (partial join ",")
                                                    (rest %)))) mat))]
      (str M " " C))))
