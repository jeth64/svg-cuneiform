(ns svg-cuneiform.core
  (:require [analemma.xml :refer [parse-xml transform-xml filter-xml emit]]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [svg-cuneiform.file :refer []]))
