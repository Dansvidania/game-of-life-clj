(ns my-sketch.core-test
  (:require [clojure.test :refer :all]
            [my-sketch.core :refer :all]))

(deftest test-update-life
  (is (= [0 0 0 0 0 0 0 0 0] (update-life [0 0 0 0 0 0 0 0 0]))))
