(ns svg-cuneiform.file
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.zip :as zip]
            [svg-cuneiform.svg :refer [parse-transform parse-path matrix2path]]
            [clojure.xml :as xml]))


(def style {:fill "none" :stroke "blue" :stroke-width 0.5})


;;
;; Helper functions
;;

(defn get-svg [path]
  (parse-xml (slurp path)))

(defn- get-layer [file layer-id]
  (filter-xml file [{:id layer-id}]))

(defn- get-transformations
  "Returns pathids-transformations list"
  [file layer-id]
  (filter #(not-any? empty? %)
   (map #(let [children (rest (first (filter-xml % [[:or :path :line]])))
               path-ids (map (partial :id) children)
               transform (parse-transform (:transform (second %)))]
           [path-ids transform])
        (filter-xml (first (get-layer file layer-id)) [:g]))))

(defn get-translations
  "Returns list of pathid-point pairs"
  [file layer-id]
  (let [transformlist (get-transformations file layer-id)]
    (reduce into '() (map #(for [id (first %) transform (second %)
                                 :when (= "translate" (first transform))]
                             [id (second transform)]) transformlist))))

(defn- translate [pathmap translations]
  (letfn [(pt-add [pt pt-list] (map (partial map + pt) pt-list))]
      (loop [paths (transient pathmap) i 0]
        (if (< i (count translations))
          (let [[id value] (nth translations i)]
            (if (not= (get paths id) nil)
              (recur (assoc! paths id (pt-add value (paths id))) (inc i))
              (recur paths (inc i))))
          (persistent! paths)))))

(defn- id-ptlist-map [file layer-id tag method translations]
  (translate (reduce into {} (map #(hash-map (:id (second %)) (method (second %)))
                                  (filter-xml (first (get-layer file layer-id)) [tag])))
             translations))


(defn get-paths [file layer-id translations] ;TODO: extract only one (first) path per group
  (id-ptlist-map file layer-id :path #(parse-path (:d %)) translations))

(defn get-lines [file layer-id translations]
  (id-ptlist-map file layer-id :line #(map (partial map read-string)
                                           [[(:x1 %) (:y1 %)] [(:x2 %) (:y2 %)]])
                 translations))


;;
;; Zipper functionalities for xml map manipulation
;;

(defn- remove-nodes [root matcher]
  (loop [loc (zip/seq-zip root)]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next (zip/remove loc)))
        (recur (zip/next loc))))))

(defn- remove-parent [root matcher]
  (loop [loc (zip/seq-zip root)]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next (zip/remove (zip/up loc))))
        (recur (zip/next loc))))))

(defn- find-layer [root group-id]
  (letfn [(certain-layer? [node]
            (and (seq? node) (= :g (first node))
                 (= (get (second node) :id) group-id)))]
      (loop [loc root]
        (if (zip/end? loc)
          loc
          (if (certain-layer? (zip/node loc))
            loc
            (recur (zip/next loc)))))))

;;
;; Higher-level functions for xml map transformations
;;

(defn- create-wedge-nodes [paths-list]
  (letfn [(wedge-node [nr pathstring]
            (list :path (merge style {:d pathstring :id (str "wedge" nr)})))]
      (map wedge-node (range) (map matrix2path paths-list))))

(defn- add-wedge-nodes [root layer-id new-nodes]
  (let [layer-loc (find-layer root layer-id)]
      (loop [i 0 loc layer-loc]
        (if (< i (count new-nodes))
          (recur (inc i) (zip/append-child loc (nth new-nodes i)))
          (zip/root loc)))))

(defn clean-file [file path-ids line-ids]
  (letfn [(certain-path? [node]
            (and (seq? node) (= :path (first node))
                 (some #(= (get (second node) :id) %) path-ids)))
          (certain-line? [node]
            (and (seq? node) (= :line (first node))
                 (some #(= (get (second node) :id) %) line-ids)))
          (empty-group? [node]
            (and (seq? node) (= :g (first node)) (= (count node) 2)))]
    (-> file
        (remove-parent certain-path?)
        (remove-nodes certain-line?)
        (remove-nodes empty-group?))
    ))


(defn update-file
  "Create path nodes for wedges, delete nodes plus parent group of used nodes
   and one layer of empty groups. (input 'paths': nx3x4x2-matrix, in right order)"
  [file layer-id paths path-ids line-ids]
  (let [nodes (create-wedge-nodes paths)]
    (-> file
        (zip/seq-zip)
        (add-wedge-nodes layer-id nodes)
        (clean-file (set (flatten path-ids)) (set (flatten line-ids))))))

(defn update-and-save
  [file layer-id paths path-ids line-ids outfile]
  (spit outfile (emit (update-file file layer-id paths path-ids line-ids))))
