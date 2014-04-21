(ns svg-cuneiform.core
  (:require [svg-cuneiform.matcher :refer [classify-and-reduce find-wedges add-extension
                                           reference ;; loeschen nach testen
                                           ]]
            [svg-cuneiform.file :refer
             [get-svg get-translations get-paths get-lines
              update-and-save update-file]]))

(def default-config
  {:filename "test/svg_cuneiform/images/1-1.svg"
   :layer-id "cuneiforms"
   :outfile "test/svg_cuneiform/images/out.svg"
   ;; parameter
   ;; - for classification
   :curve-line-threshold 0.0 ;; no curves as lines
   ;; - for finding extensions
   :extension-dist 1.0
   :cos-allowed-angle 0.5
   ;; style attributes
   :stroke-width 0.5
   :color "blue"})

(def test-config1
  {:filename "test/svg_cuneiform/images/1-1.svg"})

(def test-config2
  {:filename "test/svg_cuneiform/images/2-1.svg"
   :curve-line-threshold 0.0
   :extension-dist 0.8
   :cos-allowed-angle 0.7})

(def test-config3
  {:filename "test/svg_cuneiform/images/3-1.svg"
   ;; parameters
   :curve-line-threshold 1.0
   :extension-dist 1.0
   :cos-allowed-angle 0.6})

(def config1
  {:filename "test/svg_cuneiform/images/VAT_10833-SeiteB_HPSchaudig.svg"
   :layer-id "g20"})

(def config2
  {:filename "test/svg_cuneiform/images/VAT_09671_Rs_SJakob.svg"
   :layer-id "Autographie"})

(def config3
  {:filename "test/svg_cuneiform/images/VAT_10908_Vs.svg"
   :layer-id "Kopie"})


;;(time (main))
(time ;; defn main []
 (let [config (merge default-config test-config3)
       style {:fill "none" :stroke (config :color) :stroke-width (config :stroke-width)}
       file (get-svg (config :filename))
       translations (get-translations file (config :layer-id))
       paths (get-paths file (config :layer-id) translations)
       [curve-map line-map] (classify-and-reduce paths (config :curve-line-threshold))
       lines (merge line-map (get-lines file (config :layer-id) translations))

       a (print [(count curve-map) (count line-map)])

       [wedges used-keys] (find-wedges curve-map (config :max-merge-dist))
       [wedges2 path-keys] (add-extension wedges lines (config :extension-dist) (config :cos-allowed-angle))
       ;;c (print (add-extension wedges line-map))

       new-paths (concat wedges2
                         ;; (map #(vector (cons (map (partial + 0.5) %) (repeat 3 %))) (mapv reference (vals curve-map))) ;; Punkte
                         ;; (map #(vector (take 4 (cycle %))) (vals lines)) ;; Linien
                         ;; (map vector (vals curve-map)) ;; kurven
                          )
       paths-to-delete used-keys
       lines-to-delete []
       ]
   (update-and-save file (config :layer-id) new-paths paths-to-delete lines-to-delete (config :outfile) style)
   ))
