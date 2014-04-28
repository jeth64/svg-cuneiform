(ns svg-cuneiform.math
  (:require [incanter [stats :refer [euclidean-distance]]]))


;;
;; Basic functions
;;

(defn round
  ([decimal-places n] (read-string (format (str "%." decimal-places "f") (double n))))
  ([n] (round 0 n)))

(defn asinh [x] (Math/log (+ x (Math/sqrt (inc (* x x))))))

(defn acosh [x] (Math/log (+ x (Math/sqrt (dec (* x x))))))

(defn polynomial [coeffs x] (reduce #(+ %2 (* %1 x)) coeffs))

(defn cubic-zeros
  "Lists all real roots to Ax^3 + Bx^2 + Cx + D"
  [A B C D]
  (let [[a b c] (map #(/ (double %) A) [B C D])
        p (- b (/ (* a a) 3))
        q (+ (/ (* 2 a a a) 27) (/ (* a b) -3) c)
        z (Math/sqrt (/ (Math/abs p) 3))]
    (cond
     (= p 0) (if (= q 0)
               [(/ a -3)]
               [(/ (- (Math/cbrt (- (* a a a) (* 27 c))) a) a)])
     (> p 0) [(+ (* -2 z
                     (Math/sinh (/ (asinh (/ (* 3 q) 2 p z)) 3)))
                  (/ a -3))]
     :else (if (< (* -27 q q) (* 4 p p p))
             [(+ (* -2 z (Math/signum q)
                     (Math/cosh (/ (acosh (/ (* 3 (Math/abs q)) -2 p z)) 3)))
                  (/ a -3))]
             (mapv #(+ (* 2 z
                          (Math/cos (/ (- (Math/acos (/ (* 3 q) 2 p z))
                                          (* % 2 Math/PI)) 3)))
                      (/ a -3))
                  (range 3))))))


;;
;; Matrix and vector functions
;;

(defn euclidean-squared [u v] (reduce #(+ %1 (* %2 %2)) 0 (map - u v)))

(defn dot-product [u v] (reduce + (map * u v)) )

(defn transpose [M] (apply mapv vector M))

(defn normalize [v] (map #(/ % (euclidean-distance [0 0] v)) v))


;;
;; Statistic functions
;;

(defn average [data] (if (< (count data) 1) nil (map #(/ % (count data)) (apply map + data))))

(defn whiten [data] (map #(map - % (average data)) data))

(defn pairwise-dist
  "Returns dim(list1) x dim(list2) matrix"
  [data1 data2]
  (map #(map (partial euclidean-squared %) data2) data1))


;;
;; Specific functions
;;

(defn furthest-from-first [ptlist];; TODO: replace by "find-ends"
  (apply max-key #(euclidean-squared (first ptlist) %) ptlist))

(defn bezier-coeffs [v-part]
  [(apply + (map * [-1 3 -3 1] v-part)) ;; * t^3
   (apply + (map * [3 -6 3] v-part))    ;; * t^2
   (apply + (map * [-3 3] v-part))      ;; * t
   (first v-part)])

(defn curve-line-intersection
  "Calculates the intersection of a cubic bezier curve and a line "
  [bezier [[lx1 ly1] [lx2 ly2]]]
  (let [[[ax bx cx dx] [ay by cy dy]] (map bezier-coeffs (transpose bezier))
        A (+ (* (- ly2 ly1) ax) (* (- lx1 lx2) ay))
        B (+ (* (- ly2 ly1) bx) (* (- lx1 lx2) by))
        C (+ (* (- ly2 ly1) cx) (* (- lx1 lx2) cy))
        D (+ (* (- ly2 ly1) (- dx lx1)) (* (- lx1 lx2) (- dy ly1)))
        ts (cubic-zeros A B C D)]
    (transpose [(map #(polynomial [ax bx cx dx] %) ts)
                (map #(polynomial [ay by cy dy] %) ts)])))


(defn cubic-bezier
  "Takes control points and the value for variable t and
   evaluates corresponding cubic bezier curve for t"
  [controls t]
  (mapv #(polynomial (bezier-coeffs %) t) (transpose controls)))
