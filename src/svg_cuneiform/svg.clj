(ns svg-cuneiform.svg
  (:require [clojure.string :refer [join split trim]]
            [svg-cuneiform.math :refer [round]]))

;; Unchecked parsing: invalid transform or path strings will cause unknown behaviour


;;
;; Helper functions
;;

(defn- get-char-ptlist-pairs [str]
     (map #(vector (second %)
                   (partition 2 (map (partial read-string)
                                     (re-seq #"-?(?:\d*\.?)\d+" (last %)))))
          (re-seq #"([mMcCsSlL])([^A-Za-z]+)" str)))

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
;; Functionality for extracting points from path string
;;

(defn parse-path
  "Returns list of absolute points (length: 1+3*x)."
  [polybezier-string]
  (let [cmd-numbers-list (get-char-ptlist-pairs polybezier-string)
        first-pt (first (second (first cmd-numbers-list)))]
    (loop [offset first-pt i 0 points [first-pt]]
      (if (< i (count cmd-numbers-list))
        (let [[cmd numbers] (nth cmd-numbers-list i)
              [offset new-pts] (case cmd
                                 "M" [(last numbers) (lines2beziers (rest numbers))]
                                 "m" (make-absolute (first numbers) (lines2beziers (rest numbers)) 3)
                                 "C" [offset numbers]
                                 "c" (make-absolute offset numbers 3)
                                 "S" (add-first-cntrl-pts
                                      (map + (last points)
                                           (apply map - (reverse (take-last 2 points))))
                                      offset numbers)
                                 "s" (apply add-first-cntrl-pts
                                            (map + (last points)
                                                 (apply map - (reverse (take-last 2 points))))
                                            (make-absolute offset numbers 2))
                                 "L" [(last numbers) (lines2beziers numbers)]
                                 "l" (make-absolute offset (lines2beziers numbers) 3)
                                 )]
          (recur offset (inc i) (reduce conj points new-pts) ))
        points))))

;;(defn ptlist2path [ptlist] (matrix2path (partition 4 3 ptlist)))


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

(defn lines2beziers [line-pts]
  (apply concat (map (partial repeat 3) line-pts)))


;;
;; Functionality for producing a path-string
;;


(defn matrix2path
  "Return string for polybezier curve (input: nx4x2-matrix)"
  [matrix]
  (let [mat (map (partial map (partial map (partial round 3))) matrix)
        M (str "M" (join "," (first (first mat))))
        C (join " " (map #(str "C" (join " " (map (partial join ",")
                                                  (rest %)))) mat))]
    (str M " " C)))


;(defn path2matrix [pathstring] (partition 4 3 (parse-path pathstring)))
