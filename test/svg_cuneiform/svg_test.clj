(ns svg-cuneiform.svg-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer [facts fact]]
            [svg-cuneiform.svg :refer [parse-transform parse-path matrix2path]]))


(parse-path "m 250.342,267.458 c 0.342,1.532 1.57,2.74 1.186,4.396 -0.327,1.409 -1.111,2.8 -1.91,3.995 -0.18,0.269 0.253,0.519 0.432,0.252 0.887,-1.326 1.663,-2.831 2.025,-4.393 0.371,-1.598 -0.922,-2.913 -1.25,-4.383 -0.071,-0.315 -0.553,-0.181 -0.483,0.133 l 0,0 z")


(facts "About 'parse-transform'"
       (fact "can handle single transformation"
             (parse-transform "translate(543.654,544.65)")
             => '(["translate" (543.654 544.65)]))
       (fact "can handle multiple transformations"
             (parse-transform  "translate(543.654,544.65) scale(643, 535)")
             => '(["translate" (543.654 544.65)] ["scale" (643 535)])))

(facts "About 'parse-path'"
       (fact "can handle path with C"
             (parse-path "M100,200 C100,100 250,100 250,200 C250,300 400,300 400,200")
             => ['(100 200) '(100 100) '(250 100) '(250 200) '(250 300) '(400 300) '(400 200)])
       (fact "can handle path with c"
             (parse-path "M100,200 c0,-100 150,-100 150,0 c0,100 150,100 150,0")
             => ['(100 200) '(100 100) '(250 100) '(250 200) '(250 300) '(400 300) '(400 200)])
       (fact "can handle path with C and S"
             (parse-path  "M100,200 C100,100 250,100 250,200 S400,300 400,200")
             => ['(100 200) '(100 100) '(250 100) '(250 200) '(250 300) '(400 300) '(400 200)])
       (fact "can handle path with c and s"
             (parse-path "M100,200 c0,-100 150,-100 150,0 s150,100 150,0")
             => ['(100 200) '(100 100) '(250 100) '(250 200) '(250 300) '(400 300) '(400 200)]))

(facts "About 'matrix2path'"
       (fact "the result string has the right amount of numbers in it"
             (count (re-seq  #"-?(?:\d*\.?)\d+"
                             (matrix2path [['(100 200) '(100 100) '(250 100) '(250 200)]
                                           ['(250 200) '(250 300) '(400 300) '(400 200)]])))
             => (count (flatten ['(100 200) '(100 100) '(250 100) '(250 200) '(250 300) '(400 300) '(400 200)]))))
