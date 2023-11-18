(ns my-sketch.core-test
  (:require [clojure.test :refer :all]
            [my-sketch.core :refer :all]))

(deftest test-neighbour-pos
  ; test all possible cases in a 3x3
  (testing "given a world size of 9 and a position of 0, returns [1 3 4]"
    (is (= (set (neighbour-pos 9 0)) (set [1 3 4]))))
  (testing "given a world size of 9 and a position of 1, returns [0 2 3 4 5]"
    (is (= (set (neighbour-pos 9 1)) (set [0 2 3 4 5]))))
  (testing "given a world size of 9 and a position of 2, returns [1 4 5]"
    (is (= (set (neighbour-pos 9 2)) (set [1 4 5]))))
  (testing "given a world size of 9 and a position of 3, returns [0 1 4 6 7]"
    (is (= (set (neighbour-pos 9 3)) (set [0 1 4 6 7]))))
  (testing "given a world size of 9 and a position of 4, returns [0 1 2 3 5 6 7 8]"
    (is (= (set (neighbour-pos 9 4)) (set [0 1 2 3 5 6 7 8]))))
  (testing "given a world size of 9 and a position of 5, returns [1 2 4 7 8]"
    (is (= (set (neighbour-pos 9 5)) (set [1 2 4 7 8]))))
  (testing "given a world size of 9 and a position of 6, returns [3 4 7]"
    (is (= (set (neighbour-pos 9 6)) (set [3 4 7]))))
  (testing "given a world size of 9 and a position of 7, returns [3 4 5 6 8]"
    (is (= (set (neighbour-pos 9 7)) (set [3 4 5 6 8]))))
  (testing "given a world size of 9 and a position of 8, returns [4 5 7]"
    (is (= (set (neighbour-pos 9 8)) (set [4 5 7]))))
  ; test for positions outside of the world throwing an error
;  (testing "given a world size of 9 and a position of -1, throws an error"
;    (is (thrown? AssertionError (neighbour-pos 9 -1))))
;  (testing "given a world size of 9 and a position of 9, throws an error"
;    (is (thrown? AssertionError (neighbour-pos 9 9))))
  ; test with random world sizes for positions at the corners of the world returning 3 neighbours
  (testing "given a world size of 900 and a position of 29, returns 3 neighbours"
    (is (= (count (neighbour-pos 900 29)) 3)))
  (testing "given a world size of 900 and a position of 0, returns 3 neighbours"
    (is (= (count (neighbour-pos 900 0)) 3)))
  (testing "given a world size of 900 and a position of 870, returns 3 neighbours"
    (is (= (count (neighbour-pos 900 870)) 3)))
  (testing "given a world size of 900 and a position of 899, returns 3 neighbours"
    (is (= (count (neighbour-pos 900 899)) 3)))
  ; test with random world sizes for positions at the edges of the world returning 5 neighbours
  (testing "given a world of size 900 and a position of 59 returns 5 neighbours"
    (is (= (count (neighbour-pos 900 59)) 5)))
  ; test with random world sizes for positions in the middle of the world returning 8 neighbours
  (testing "given a world size of 900 and a position of 87, returns 8 neighbours"
    (is (= (count (neighbour-pos 900 87)) 8)))
  (testing "given a world size of 900 and a position of 868, returns 8 neighbours"
    (is (= (count (neighbour-pos 900 868)) 8)))
  ; test with random world sizes for positions outside of the world throwing an error 
;  (testing "given a world size of 100 and a position of -1, throws an error"
;    (is (thrown? AssertionError (neighbour-pos 10000 -1))))
;  (testing "given a world size of 100 and a position of 10000, throws an error"
;    (is (thrown? AssertionError (neighbour-pos 10000 10000))))
  )

(deftest test-evaluate-cell-life
  (testing "correctly calculates life for edge cells"
    (is (= (evaluate-cell-life 9 [true  false false
                                  false false false
                                  false false false]
                               0)
           false)))
  (testing "given a world with no neighbours, returns false"
    (is (= (evaluate-cell-life 9 [false false false
                                  false true false
                                  false false false]
                               4)
           false))))

(def world {:world [false false false
                    false false false
                    false false false]
            :total-count 9
            :pos-range (range 9)
            :side-count 3})

(deftest test-update-life
  (testing "given an empty world, returns an empty world"
    (is (= (update-life world)
           world)))
  (testing "given a world with one cell, returns an empty world")
  (testing "given a world with 4 neighbouring cells, returns the same world")
  (testing "given a world with 5 neighbouring cells, returns the same world")
  (testing "given a world with 6 neighbouring cells, returns the same world"))
