(ns svg-cuneiform.core
  (:require [svg-cuneiform.matcher :refer :all]
            [svg-cuneiform.file :refer
             [get-svg get-translations get-paths get-lines
              update-and-save update-file]]))


(time (main))
(defn main []
  (let [[layer-id filename]
        ["cuneiforms" "test/svg_cuneiform/images/3-1.svg"]
       ;; ["g20" "test/svg_cuneiform/images/VAT_10833-SeiteB_HPSchaudig.svg"]
        file (get-svg filename)
        outfile "test/svg_cuneiform/images/out.svg"
        translations (get-translations file layer-id)
        [curve-map line-map] (classify-paths (get-paths file layer-id translations) 1.0)

        [wedges used-keys] (get-wedges3 curve-map)
        ;; [wedges path-keys line-keys] (add-extension wedges line-map)

        paths-to-delete used-keys
        lines-to-delete []
        ]

     (update-and-save file layer-id wedges paths-to-delete lines-to-delete outfile)
    ;;line-map
    )
  )
