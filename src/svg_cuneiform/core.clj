(ns svg-cuneiform.core
  (:require [svg-cuneiform.matcher :refer :all]
            [svg-cuneiform.file :refer
             [get-svg get-translations get-paths get-lines
              update-and-save update-file]]))


;;(time (main))
(time ;; defn main []
  (let [[layer-id filename]
        ["cuneiforms" "test/svg_cuneiform/images/3-1.svg"]
       ;; ["g20" "test/svg_cuneiform/images/VAT_10833-SeiteB_HPSchaudig.svg"]
        file (get-svg filename)
        outfile "test/svg_cuneiform/images/out.svg"
        translations (get-translations file layer-id)
        [curve-map line-map] (classify-and-reduce (get-paths file layer-id translations) 1.0)


        [wedges used-keys] (find-wedges curve-map)
        ;; [wedges2 path-keys] (add-extension wedges line-map)
        c (print (add-extension wedges line-map))

        new-paths (concat wedges (map #(vector (cons (map (partial + 0.5) %) (repeat 3 %)))
                                      (mapv intersection (vals curve-map))) ;; Punkte
                          (map #(vector (take 4 (cycle %))) (vals line-map)) ;; Linien
                          )
        paths-to-delete used-keys
        lines-to-delete []
        ]

    ;;(update-and-save file layer-id new-paths paths-to-delete lines-to-delete outfile)

    ;(f (get-paths file layer-id translations))

    ))
