(ns ocr-lib.core-test
  (:require [clojure.test :refer :all]
            [ocr-lib.core :refer :all])
  (:import [java.util Base64]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io FileInputStream
                    File
                    ByteArrayInputStream
                    ByteArrayOutputStream]))

(deftest test-process-images-calculate-progress-value-fn
  (testing "Test process images calculate progress value fn"
    
    (reset!
      process-images-progress-bar-value
      nil)
    
    (reset!
      total-pixels-value
      nil)
    
    (let [result (process-images-calculate-progress-value-fn)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (reset!
      process-images-progress-bar-value
      nil)
    
    (reset!
      total-pixels-value
      10)
    
    (let [result (process-images-calculate-progress-value-fn)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (reset!
      process-images-progress-bar-value
      5)
    
    (reset!
      total-pixels-value
      10)
    
    (let [result (process-images-calculate-progress-value-fn)]
      
      (is
        (= result
           50)
       )
      
     )
    
    (reset!
      process-images-progress-bar-value
      11)
    
    (reset!
      total-pixels-value
      10)
    
    (let [result (process-images-calculate-progress-value-fn)]
      
      (is
        (= result
           0)
       )
      
     )
    
   ))

(deftest test-process-images-reset-progress-value-fn
  (testing "Test process images reset progress value fn"
    
    (reset!
      process-images-progress-bar-value
      nil)
    
    (reset!
      total-pixels-value
      nil)
    
    (let [result (process-images-reset-progress-value-fn)]
      
      (is
        (= @process-images-progress-bar-value
           0)
       )
      
      (is
        (= @total-pixels-value
           0)
       )
      
     )
    
    (reset!
      process-images-progress-bar-value
      "")
    
    (reset!
      total-pixels-value
      "")
    
    (let [result (process-images-reset-progress-value-fn)]
      
      (is
        (= @process-images-progress-bar-value
           0)
       )
      
      (is
        (= @total-pixels-value
           0)
       )
      
     )
    
    (reset!
      process-images-progress-bar-value
      5)
    
    (reset!
      total-pixels-value
      5)
    
    (let [result (process-images-reset-progress-value-fn)]
      
      (is
        (= @process-images-progress-bar-value
           0)
       )
      
      (is
        (= @total-pixels-value
           0)
       )
      
     )
    
   ))

(deftest test-increase-total-pixels
  (testing "Test increase total pixels"
    
    (reset!
      total-pixels-value
      nil)
    
    (let [width nil
          height nil
          result (increase-total-pixels
                   width
                   height)]
      
      (is
        (nil?
          @total-pixels-value)
       )
      
     )
    
    (reset!
      total-pixels-value
      nil)
    
    (let [width 10
          height nil
          result (increase-total-pixels
                   width
                   height)]
      
      (is
        (nil?
          @total-pixels-value)
       )
      
     )
    
    (reset!
      total-pixels-value
      nil)
    
    (let [width 10
          height 10
          result (increase-total-pixels
                   width
                   height)]
      
      (is
        (nil?
          @total-pixels-value)
       )
      
     )
    
    (reset!
      total-pixels-value
      0)
    
    (let [width 10
          height 10
          result (increase-total-pixels
                   width
                   height)]
      
      (is
        (not
          (nil?
            @total-pixels-value))
       )
      
      (is
        (= @total-pixels-value
           100)
       )
      
     )
    
   ))

(deftest test-read-base64-image-fn
  (testing "Test read base64 image fn"
    
    (let [image-base64 nil
          result (read-base64-image-fn
                   image-base64)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-base64 ""
          result (read-base64-image-fn
                   image-base64)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-base64 "test"
          result (read-base64-image-fn
                   image-base64)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [base64-encoder (Base64/Encoder/getEncoder)
          image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          image-base64 (.encodeToString
                         base64-encoder
                         image-byte-array)
          result (read-base64-image-fn
                   image-base64)]
      
      (is
        (instance?
          java.awt.image.BufferedImage
          result)
       )
      
     )
    
    
   ))

(deftest test-contrast-fn
  (testing "Test contrast fn"
    
    (let [color nil
          contrast-value nil
          result (contrast-fn
                   color
                   contrast-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [color ""
          contrast-value ""
          result (contrast-fn
                   color
                   contrast-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [color 1
          contrast-value 1
          result (contrast-fn
                   color
                   contrast-value)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [color 1
          contrast-value 2
          result (contrast-fn
                   color
                   contrast-value)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [color 4
          contrast-value 2
          result (contrast-fn
                   color
                   contrast-value)]
      
      (is
        (= result
           2)
       )
      
     )
    
    (let [color 128
          contrast-value 1
          result (contrast-fn
                   color
                   contrast-value)]
      
      (is
        (= result
           129)
       )
      
     )
    
    (let [color 128
          contrast-value 128
          result (contrast-fn
                   color
                   contrast-value)]
      
      (is
        (= result
           255)
       )
      
     )
    
   ))

(deftest test-grayscale-contrast-fn
  (testing "Test grayscale contrast fn"
    
    (let [image nil
          light-value nil
          contrast-value nil
          result (grayscale-contrast-fn
                   image
                   light-value
                   contrast-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (grayscale-contrast-fn
                   image
                   light-value
                   contrast-value)]
      
      (is
        (instance?
          BufferedImage
          result)
       )
      
      (let [height (.getHeight
                     result)
            width (.getWidth
                    result)]
        (dotimes [x width]
          (dotimes [y height]
            
            (let [p (.getRGB
                      image
                      x
                      y)
                  r (bit-and ; red
                      (bit-shift-right
                        p
                        16)
                      255)
                  g (bit-and ; green
                      (bit-shift-right
                        p
                        8)
                      255)
                  b (bit-and ; blue
                      (bit-shift-right
                        p
                        0)
                      255)]
              
              (is
                (contains?
                  #{0
                    255}
                  r)
               )
              
              (is
                (contains?
                  #{0
                    255}
                  g)
               )
              
              (is
                (contains?
                  #{0
                    255}
                  b)
               )
              
             )
            
           ))
       )
      
     )
    
   ))

(deftest test-read-black-dots-fn
  (testing "Test read black dots fn"
    
    (let [image nil
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)]
      
      (is
        (set?
          result)
       )
      
      (is
        (= 378
           (count
             result))
       )
     
     )
    
   ))

(deftest test-read-black-dots-part-fn
  (testing "Test read black dots part fn"
    
    (let [image nil
          start-y nil
          end-y nil
          light-value nil
          contrast-value nil
          result (read-black-dots-part-fn
                   image
                   start-y
                   end-y
                   light-value
                   contrast-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          start-y nil
          end-y nil
          light-value nil
          contrast-value nil
          result (read-black-dots-part-fn
                   image
                   start-y
                   end-y
                   light-value
                   contrast-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          start-y 1
          end-y 1
          light-value nil
          contrast-value nil
          result (read-black-dots-part-fn
                   image
                   start-y
                   end-y
                   light-value
                   contrast-value)]
      
      (is
        (set?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          start-y 5
          end-y 1
          light-value nil
          contrast-value nil
          result (read-black-dots-part-fn
                   image
                   start-y
                   end-y
                   light-value
                   contrast-value)]
      
      (is
        (set?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          start-y 0
          end-y 25
          light-value nil
          contrast-value nil
          result (read-black-dots-part-fn
                   image
                   start-y
                   end-y
                   light-value
                   contrast-value)]
      
      (is
        (set?
          result)
       )
      
      (is
        (= (count
             result)
           187)
       )
      
     )
    
   ))

(deftest test-dot-is-black?
  (testing "Test dot is black?"
    
    (let [image nil
          x nil
          y nil
          result (dot-is-black?
                   image
                   x
                   y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          x nil
          y nil
          result (dot-is-black?
                   image
                   x
                   y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          image (grayscale-contrast-fn
                  image)
          x 5
          y 5
          result (dot-is-black?
                   image
                   x
                   y)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          image (grayscale-contrast-fn
                  image)
          x 14
          y 20
          result (dot-is-black?
                   image
                   x
                   y)]
      
      (is
        (true?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          image (grayscale-contrast-fn
                  image)
          x 35
          y 20
          result (dot-is-black?
                   image
                   x
                   y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          image (grayscale-contrast-fn
                  image)
          x 14
          y 60
          result (dot-is-black?
                   image
                   x
                   y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-neighbour-dots-set-fn
  (testing "Test neighbour dots set fn"
    
    (let [x nil
          y nil
          result (neighbour-dots-set-fn
                   x
                   y)]
      
      (is
        (set?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [x 1
          y 2
          result (neighbour-dots-set-fn
                   x
                   y)]
      
      (is
        (set?
          result)
       )
      
      (is
        (not
          (empty?
            result))
       )
      
      (is
        (= result
           #{[(dec 1) 2]
             [(inc 1) 2]
             [1 (dec 2)]
             [1 (inc 2)]
             [(dec 1) (inc 2)]
             [(inc 1) (inc 2)]
             [(inc 1) (dec 2)]
             [(dec 1) (dec 2)]})
       )
      
     )
    
    (let [x 5
          y 8
          result (neighbour-dots-set-fn
                   x
                   y)]
      
      (is
        (set?
          result)
       )
      
      (is
        (not
          (empty?
            result))
       )
      
      (is
        (= result
           #{[(dec 5) 8]
             [(inc 5) 8]
             [5 (dec 8)]
             [5 (inc 8)]
             [(dec 5) (inc 8)]
             [(inc 5) (inc 8)]
             [(inc 5) (dec 8)]
             [(dec 5) (dec 8)]})
       )
      
     )
    
   ))

(deftest test-find-sign-dots-fn
  (testing "Test find sign dots fn"
    
    (let [all-dots nil
          sign-dots nil
          checked-dots nil
          width nil
          height nil
          result (find-sign-dots-fn
                   all-dots
                   sign-dots
                   checked-dots
                   width
                   height)]
      
      (is
        (= result
           [nil nil])
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)
          all-dots result
          sign-dots nil
          checked-dots nil
          width nil
          height nil
          [result-all-dots
           result-sign-dots] (find-sign-dots-fn
                               all-dots
                               sign-dots
                               checked-dots
                               width
                               height)]
      
      (is
        (= (count
             result-all-dots)
           378)
       )
      
      (is
        (nil?
          sign-dots)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)
          all-dots result
          sign-dots #{}
          checked-dots nil
          width nil
          height nil
          [result-all-dots
           result-sign-dots] (find-sign-dots-fn
                               all-dots
                               sign-dots
                               checked-dots
                               width
                               height)]
      
      (is
        (= (count
             result-all-dots)
           378)
       )
      
      (is
        (set?
          result-sign-dots)
       )
      
      (is
        (empty?
          result-sign-dots)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)
          all-dots result
          [x y] (first
                  all-dots)
          all-dots (disj
                     all-dots
                     [x y])
          sign-dots #{[x y]}
          checked-dots nil
          width nil
          height nil
          [result-all-dots
           result-sign-dots] (find-sign-dots-fn
                               all-dots
                               sign-dots
                               checked-dots
                               width
                               height)]
      
      (is
        (= (count
             result-all-dots)
           377)
       )
      
      (is
        (set?
          result-sign-dots)
       )
      
      (is
        (= (first
             result-sign-dots)
           [23 38])
        
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)
          all-dots result
          [x y] (first
                  all-dots)
          all-dots (disj
                     all-dots
                     [x y])
          sign-dots #{[x y]}
          checked-dots #{}
          width nil
          height nil
          [result-all-dots
           result-sign-dots] (find-sign-dots-fn
                               all-dots
                               sign-dots
                               checked-dots
                               width
                               height)]
      
      (is
        (= (count
             result-all-dots)
           377)
       )
      
      (is
        (set?
          result-sign-dots)
       )
      
      (is
        (= (first
             result-sign-dots)
           [23 38])
        
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)
          all-dots result
          [x y] (first
                  all-dots)
          all-dots (disj
                     all-dots
                     [x y])
          sign-dots #{[x y]}
          checked-dots #{}
          width (.getWidth
                  image)
          height (.getHeight
                   image)
          [result-all-dots
           result-sign-dots] (find-sign-dots-fn
                               all-dots
                               sign-dots
                               checked-dots
                               width
                               height)]
      
      (is
        (= (count
             result-all-dots)
           73)
       )
      
      (is
        (set?
          result-sign-dots)
       )
      
      (is
        (= (count
             result-sign-dots)
           305)
        
       )
      
     )
    
   ))

(deftest test-find-min-max-fn
  (testing "Test find min max fn"
    
    (let [dots-set nil
          result (find-min-max-fn
                   dots-set)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dots-set #{}
          result (find-min-max-fn
                   dots-set)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dots-set #{5 6}
          result (find-min-max-fn
                   dots-set)]
      
      (is
        (= result
           [Integer/MAX_VALUE Integer/MAX_VALUE
            0 0])
       )
      
     )
    
    (let [dots-set #{5 6 [5 5]}
          result (find-min-max-fn
                   dots-set)]
      
      (is
        (= result
           [5 5 5 5])
       )
      
     )
    
    (let [dots-set #{[5 5]}
          result (find-min-max-fn
                   dots-set)]
      
      (is
        (= result
           [5 5 5 5])
       )
      
     )
    
    (let [dots-set #{[4 4] [5 5] [6 6] [9 9]}
          result (find-min-max-fn
                   dots-set)]
      
      (is
        (= result
           [4 4 9 9])
       )
      
     )
    
   ))

(deftest test-find-dots-hooks-fn
  (testing "Test find dots hooks fn"
    
    (let [all-dots nil
          x-min nil
          x-max nil
          y-min nil
          y-max nil
          hooks-value nil
          result (find-dots-hooks-fn
                   all-dots
                   x-min
                   x-max
                   y-min
                   y-max
                   hooks-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          result (read-black-dots-fn
                   image
                   light-value
                   contrast-value)
          all-dots result
          [x y] (first
                  all-dots)
          all-dots (disj
                     all-dots
                     [x y])
          sign-dots #{[x y]}
          checked-dots #{}
          width (.getWidth
                  image)
          height (.getHeight
                   image)
          [result-all-dots
           result-sign-dots] (find-sign-dots-fn
                               all-dots
                               sign-dots
                               checked-dots
                               width
                               height)
          all-dots result-all-dots
          [x-min y-min
           x-max y-max] (find-min-max-fn
                          result-sign-dots)
          hooks-value 8
          result (find-dots-hooks-fn
                   all-dots
                   x-min
                   x-max
                   y-min
                   y-max
                   hooks-value)]
      
      (is
        (= (count
             result)
           56)
       )
      
     )
    
   ))

(deftest test-sort-row-elements
  (testing "Test sort row elements"
    
    (let [param1 nil
          param2 nil
          result (sort-row-elements
                   param1
                   param2)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [param1 [[0 28] "Element value 1"]
          param2 [[2 25] "Element value 2"]
          result (sort-row-elements
                   param1
                   param2)]
      
      (is
        (true?
          result)
       )
      
     )
    
    (let [param1 [[2 28] "Element value 1"]
          param2 [[0 25] "Element value 2"]
          result (sort-row-elements
                   param1
                   param2)]
      
      (is
        (false?
          result)
       )
      
     )
    
   ))

(deftest test-sort-rows
  (testing "Test sort rows"
    
    (let [param1 nil
          param2 nil
          result (sort-rows
                   param1
                   param2)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [param1 [[3 5] "Element value 1"]
          param2 [[6 8] "Element value 2"]
          result (sort-rows
                   param1
                   param2)]
      
      (is
        (true?
          result)
       )
      
     )
    
    (let [param1 [[3 6] "Element value 1"]
          param2 [[4 8] "Element value 2"]
          result (sort-rows
                   param1
                   param2)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (let [param1 [[6 8] "Element value 1"]
          param2 [[3 4] "Element value 2"]
          result (sort-rows
                   param1
                   param2)]
      
      (is
        (false?
          result)
       )
      
     )
    
   ))

(deftest test-add-into-sorted-set-fn
  (testing "Test add into sorted set fn"
    
    (let [dots-sets-of-signs nil
          x-min nil
          y-min nil
          x-max nil
          y-max nil
          sign-dots nil
          result (add-into-sorted-set-fn
                   dots-sets-of-signs
                   x-min
                   y-min
                   x-max
                   y-max
                   sign-dots)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dots-sets-of-signs #{}
          x-min 5
          y-min 5
          x-max 20
          y-max 20
          sign-dots #{}
          result (add-into-sorted-set-fn
                   dots-sets-of-signs
                   x-min
                   y-min
                   x-max
                   y-max
                   sign-dots)]
      
      (is
        (= result
           #{[[5 20] #{[[5 20] #{}]}]})
       )
      
     )
    
    (let [dots-sets-of-signs #{[[5 20] #{[[5 20] #{}]}]
                               [[25 40] #{[[5 20] #{}]}]
                               [[45 60] #{[[5 20] #{}]}]}
          x-min 25
          y-min 26
          x-max 50
          y-max 40
          sign-dots #{}
          result (add-into-sorted-set-fn
                   dots-sets-of-signs
                   x-min
                   y-min
                   x-max
                   y-max
                   sign-dots)]
      
      (is
        (= result
           #{[[5 20] #{[[5 20] #{}]}]
             [[45 60] #{[[5 20] #{}]}]
             [[25 40] #{[[5 20] #{}]
                        [[25 50] #{}]}]})
       )
      
     )
    
    (let [dots-sets-of-signs #{[[5 20] #{[[5 20] #{}]}]
                               [[25 40] #{[[5 20] #{}]}]
                               [[45 60] #{[[5 20] #{}]}]}
          x-min 5
          y-min 65
          x-max 20
          y-max 80
          sign-dots #{}
          result (add-into-sorted-set-fn
                   dots-sets-of-signs
                   x-min
                   y-min
                   x-max
                   y-max
                   sign-dots)]
      
      (is
        (= result
           #{[[5 20] #{[[5 20] #{}]}]
             [[45 60] #{[[5 20] #{}]}]
             [[25 40] #{[[5 20] #{}]}]
             [[65 80] #{[[5 20] #{}]}]})
       )
      
     )
    
   ))

(deftest test-grouping-dots-fn
  (testing "Test grouping dots fn"
    
    (let [all-dots nil
          dots-sets-of-signs nil
          width nil
          height nil
          hooks-value nil
          result (grouping-dots-fn
                   all-dots
                   dots-sets-of-signs
                   width
                   height
                   hooks-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [all-dots #{}
          dots-sets-of-signs (sorted-set-by
                               sort-rows)
          width 10
          height 10
          hooks-value 8
          result (grouping-dots-fn
                   all-dots
                   dots-sets-of-signs
                   width
                   height
                   hooks-value)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [all-dots #{}
          dots-sets-of-signs (sorted-set-by
                               sort-rows)
          width 10
          height 10
          hooks-value 8
          result (grouping-dots-fn
                   all-dots
                   dots-sets-of-signs
                   width
                   height
                   hooks-value)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [all-dots #{[20 25]}
          dots-sets-of-signs (sorted-set-by
                               sort-rows)
          width 10
          height 10
          hooks-value 8
          result (grouping-dots-fn
                   all-dots
                   dots-sets-of-signs
                   width
                   height
                   hooks-value)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (set?
          result)
       )
      
      (is
        (= result
           #{[[25 25] #{[[20 20] #{[20 25]}]}]})
       )
      
     )
    
    (let [all-dots #{[20 25] [21 25] [22 26]}
          dots-sets-of-signs (sorted-set-by
                               sort-rows)
          width 10
          height 10
          hooks-value 8
          result (grouping-dots-fn
                   all-dots
                   dots-sets-of-signs
                   width
                   height
                   hooks-value)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (set?
          result)
       )
      
      (is
        (= result
           #{[[25 25] #{[[20 20] #{[20 25]}]
                        [[21 21] #{[21 25]}]}
              ]
             [[26 26] #{[[22 22] #{[22 26]}]}]
             })
       )
      
     )
    
   ))

(deftest test-read-signs-fn
  (testing "Test read signs fn"
    
    (let [signs nil
          light-value nil
          contrast-value nil
          result (read-signs-fn
                   signs
                   light-value
                   contrast-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [signs {}
          light-value nil
          contrast-value nil
          result (read-signs-fn
                   signs
                   light-value
                   contrast-value)]
      
      (is
        (map?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          signs {:i image-byte-array}
          light-value nil
          contrast-value nil
          result (read-signs-fn
                   signs
                   light-value
                   contrast-value)]
      
      (is
        (map?
          result)
       )
      
      (is
        (= result
           {:i []})
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          signs {:i [image-byte-array]}
          light-value nil
          contrast-value nil
          result (read-signs-fn
                   signs
                   light-value
                   contrast-value)]
      
      (is
        (map?
          result)
       )
      
      (is
        (contains?
          result
          :i)
       )
      
      (is
        (= (count
             (first
               (:i result))
            )
           378)
       )
      
     )
    
   ))

(deftest test-bring-to-zero-coordinates-fn
  (testing "Test bring to zero coordinates fn"
    
    (let [dots-set nil
          result (bring-to-zero-coordinates-fn
                   dots-set)]
      
      (is
        (set?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [dots-set #{[4 5] [5 6] [6 7]}
          result (bring-to-zero-coordinates-fn
                   dots-set)]
      
      (is
        (set?
          result)
       )
      
      (is
        (= result
           #{[2 2] [0 0] [1 1]})
       )
      
     )
    
    (let [dots-set #{1 [5 6] [6 7]}
          result (bring-to-zero-coordinates-fn
                   dots-set)]
      
      (is
        (set?
          result)
       )
      
      (is
        (= result
           #{[0 0] [1 1]})
       )
      
     )
    
   ))

(deftest test-check-matching-fn
  (testing "Test check matching fn"
    
    (let [zero-coordinates nil
          read-signs nil
          result (check-matching-fn
                   zero-coordinates
                   read-signs)]
      
      (is
        (= result
           {:sign "", :matching 0})
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          black-dots (read-black-dots-fn
                       image
                       light-value
                       contrast-value)
          zero-cordinates-dots (bring-to-zero-coordinates-fn
                                 black-dots)
          zero-coordinates zero-cordinates-dots
          read-signs nil
          result (check-matching-fn
                   zero-coordinates
                   read-signs)]
      
      (is
        (= result
           {:sign "", :matching 0})
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          black-dots (read-black-dots-fn
                       image
                       light-value
                       contrast-value)
          zero-cordinates-dots (bring-to-zero-coordinates-fn
                                 black-dots)
          zero-coordinates zero-cordinates-dots
          read-signs {:i zero-cordinates-dots}
          result (check-matching-fn
                   zero-coordinates
                   read-signs)]
      
      
      (is
        (= result
           {:sign "", :matching 0})
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          light-value nil
          contrast-value nil
          black-dots (read-black-dots-fn
                       image
                       light-value
                       contrast-value)
          zero-cordinates-dots (bring-to-zero-coordinates-fn
                                 black-dots)
          zero-coordinates zero-cordinates-dots
          read-signs {:i [zero-cordinates-dots]}
          result (check-matching-fn
                   zero-coordinates
                   read-signs)]
      
      
      (is
        (= result
           {:sign "i", :matching 100.0})
       )
      
     )
    
   ))

(deftest test-draw-sign
  (testing "Test draw sign"
    
    (let [image nil
          coordinates-set nil
          result (draw-sign
                   image
                   coordinates-set)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          image-is (FileInputStream.
                     (File.
                       image-path))
          image-i (ImageIO/read
                    image-is)
          light-value nil
          contrast-value nil
          image-i (grayscale-contrast-fn
                    image-i
                    light-value
                    contrast-value)
          black-dots (read-black-dots-fn
                       image
                       light-value
                       contrast-value)
          coordinates-set black-dots
          result (draw-sign
                   image
                   coordinates-set)]
      
      (is
        (= (.getWidth
             image-i)
           (.getWidth
             result))
       )
      
      (is
        (= (.getHeight
             image-i)
           (.getHeight
             result))
       )
      
      (dotimes [x (.getWidth
                    result)]
        (dotimes [y (.getHeight
                      result)]
          
          (let [result-p (.getRGB
                           result
                           x
                           y)
                image-i-p (.getRGB
                            image-i
                            x
                            y)]
            (is
              (= result-p
                 image-i-p)
             )
           )
          
         )
       )
      
     )
    
   ))

(deftest test-grouping-dots-one-part-fn
  (testing "Test grouping dots one part fn"
    
    (let [image nil
          thread-number nil
          n-threads nil
          light-value nil
          contrast-value nil
          hooks-value nil
          result (grouping-dots-one-part-fn
                   image
                   thread-number
                   n-threads
                   light-value
                   contrast-value
                   hooks-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          image (ImageIO/read
                  image-is)
          thread-number 0
          n-threads 1
          light-value nil
          contrast-value nil
          hooks-value 8
          result (grouping-dots-one-part-fn
                   image
                   thread-number
                   n-threads
                   light-value
                   contrast-value
                   hooks-value)]
      
      (is
        (= (count
             (get
               (first
                 (get
                   (first
                     (rest
                       result))
                   1))
               1))
           373)
       )
      
     )
    
   ))

(deftest test-merge-and-disj
  (testing "Test merge and disj"
    
    (let [selected-signs-set nil
          new-x-min nil
          new-x-max nil
          new-sign nil
          row nil
          result (merge-and-disj
                   selected-signs-set
                   new-x-min
                   new-x-max
                   new-sign
                   row)]
      (is
        (nil?
          result)
       )
     )
    
    (let [selected-signs-set #{}
          new-x-min nil
          new-x-max nil
          new-sign nil
          row nil
          result (merge-and-disj
                   selected-signs-set
                   new-x-min
                   new-x-max
                   new-sign
                   row)]
      (is
        (nil?
          result)
       )
     )
    
    (let [selected-signs-set #{[[1 2] "sign"]}
          new-x-min nil
          new-x-max nil
          new-sign nil
          row nil
          result (merge-and-disj
                   selected-signs-set
                   new-x-min
                   new-x-max
                   new-sign
                   row)]
      (is
        (nil?
          result)
       )
     )
    
    (let [selected-signs-set #{[[1 2] "sign"]}
          new-x-min Integer/MAX_VALUE
          new-x-max 0
          new-sign #{}
          row nil
          result (merge-and-disj
                   selected-signs-set
                   new-x-min
                   new-x-max
                   new-sign
                   row)]
      (is
        (not
          (nil?
            result))
       )
     )
    
    (let [selected-signs-set #{[[1 2] #{[1 2] [3 4]}]}
          new-x-min Integer/MAX_VALUE
          new-x-max 0
          new-sign #{[5 6]}
          row nil
          [result-new-x-min
           result-new-x-max
           result-new-sign
           result-row] (merge-and-disj
                         selected-signs-set
                         new-x-min
                         new-x-max
                         new-sign
                         row)]
      
      (is
        (= result-new-x-min
           1)
       )
      
      (is
        (= result-new-x-max
           2)
       )
      
      (is
        (= result-new-sign
           #{[3 4] [1 2] [5 6]})
       )
      
      (is
        (nil?
          result-row)
       )
      
     )
    
    (let [selected-signs-set #{[[1 2] #{[1 2] [3 4]}]}
          new-x-min Integer/MAX_VALUE
          new-x-max 0
          new-sign #{[5 6]}
          row #{[[1 2] #{[1 2] [3 4]}]}
          [result-new-x-min
           result-new-x-max
           result-new-sign
           result-row] (merge-and-disj
                         selected-signs-set
                         new-x-min
                         new-x-max
                         new-sign
                         row)]
      
      (is
        (= result-new-x-min
           1)
       )
      
      (is
        (= result-new-x-max
           2)
       )
      
      (is
        (= result-new-sign
           #{[3 4] [1 2] [5 6]})
       )
      
      (is
        (set?
          result-row)
       )
      
      (is
        (empty?
          result-row)
       )
      
     )
    
   ))

(deftest test-merge-sign-coordinates-concrete
  (testing "Test merge sign coordinates concrete"
    
    (let [row-f nil
          row-s nil
          new-sign nil
          new-x-min nil
          new-x-max nil
          [result] (merge-sign-coordinates-concrete
                     row-f
                     row-s
                     new-sign
                     new-x-min
                     new-x-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [row-f #{}
          row-s #{}
          new-sign nil
          new-x-min nil
          new-x-max nil
          [result] (merge-sign-coordinates-concrete
                     row-f
                     row-s
                     new-sign
                     new-x-min
                     new-x-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [row-f #{}
          row-s #{}
          new-sign nil
          new-x-min 0
          new-x-max 25
          [result] (merge-sign-coordinates-concrete
                     row-f
                     row-s
                     new-sign
                     new-x-min
                     new-x-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [row-f #{}
          row-s #{}
          new-sign #{}
          new-x-min 0
          new-x-max 25
          [result] (merge-sign-coordinates-concrete
                     row-f
                     row-s
                     new-sign
                     new-x-min
                     new-x-max)]
      
      (is
        (= result
           [[0 25] #{}])
       )
      
     )
    
    (let [row-f #{[[2 23] #{[2 23] [3 23] [4 22]}]}
          row-s #{[[3 22] #{[3 20] [4 19]}]}
          new-sign #{}
          new-x-min 0
          new-x-max 25
          [result] (merge-sign-coordinates-concrete
                     row-f
                     row-s
                     new-sign
                     new-x-min
                     new-x-max)]
      
      (is
        (= result
           [[0 25] #{[4 19] [4 22] [2 23] [3 20] [3 23]}] )
       )
      
     )
    
    (let [row-f #{[[2 23] #{[2 23] [3 23] [4 22]}]}
          row-s #{[[3 22] #{[3 20] [4 19]}]}
          new-sign #{[5 15]}
          new-x-min 0
          new-x-max 25
          [result] (merge-sign-coordinates-concrete
                     row-f
                     row-s
                     new-sign
                     new-x-min
                     new-x-max)]
      
      (is
        (= result
           [[0 25] #{[5 15] [4 19] [4 22] [2 23] [3 20] [3 23]}])
       )
      
     )
    
   ))

(deftest test-merge-sign-coordinates
  (testing "Test merge sign coordinates"
    
    (let [row-f nil
          row-s nil
          result-param nil
          new-sign nil
          new-x-min nil
          new-x-max nil
          result (merge-sign-coordinates
                   row-f
                   row-s
                   result-param
                   new-sign
                   new-x-min
                   new-x-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [row-f #{}
          row-s #{}
          result-param nil
          new-sign nil
          new-x-min nil
          new-x-max nil
          result (merge-sign-coordinates
                   row-f
                   row-s
                   result-param
                   new-sign
                   new-x-min
                   new-x-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [row-f #{}
          row-s #{}
          result-param nil
          new-sign nil
          new-x-min 0
          new-x-max 25
          result (merge-sign-coordinates
                   row-f
                   row-s
                   result-param
                   new-sign
                   new-x-min
                   new-x-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [row-f #{}
          row-s #{}
          result-param #{}
          new-sign #{}
          new-x-min 0
          new-x-max 25
          result (merge-sign-coordinates
                   row-f
                   row-s
                   result-param
                   new-sign
                   new-x-min
                   new-x-max)]
      
      (is
        (= result
           #{})
       )
      
     )
    
    (let [row-f #{[[2 23] #{[2 23] [3 23] [4 22]}]}
          row-s #{[[3 22] #{[3 20] [4 19]}]}
          result-param #{}
          new-sign #{}
          new-x-min 0
          new-x-max 25
          result (merge-sign-coordinates
                   row-f
                   row-s
                   result-param
                   new-sign
                   new-x-min
                   new-x-max)]
      
      (is
        (= result
           #{[[2 23] #{[4 19] [4 22] [2 23] [3 20] [3 23]}]} )
       )
      
     )
    
    (let [row-f #{[[2 23] #{[2 23] [3 23] [4 22]}]}
          row-s #{[[3 22] #{[3 20] [4 19]}]}
          result-param #{}
          new-sign #{[5 15]}
          new-x-min 0
          new-x-max 25
          result (merge-sign-coordinates
                   row-f
                   row-s
                   result-param
                   new-sign
                   new-x-min
                   new-x-max)]
      
      (is
        (= result
           #{[[2 23] #{[4 19] [4 22] [2 23] [3 20] [3 23]}]})
       )
      
     )
    
   ))

(deftest test-merge-separated-parts
  (testing "Test merge separated parts"
    
    (let [sorted-rows-set nil
          result (merge-separated-parts
                   sorted-rows-set)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [sorted-rows-set #{}
          result (merge-separated-parts
                   sorted-rows-set)]
      
      (is
        (set?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [sorted-rows-set (sorted-set-by
                            sort-rows
                            [[1 3]
                              (sorted-set-by
                                sort-row-elements
                                [[2 23] #{[2 23] [3 23] [4 22]}])
                             ])
          sorted-rows-set (conj
                            sorted-rows-set
                            [[4 7]
                              (sorted-set-by
                                sort-row-elements
                                [[3 22] #{[3 20] [4 19]}])
                             ]
                            [[8 10]
                              (sorted-set-by
                                sort-row-elements
                                [[4 20] #{[4 19] [5 18]}])
                             ])
          result (merge-separated-parts
                   sorted-rows-set)]
      
      (is
        (set?
          result)
       )
      
      (let [[[y-row-min
              y-row-max] row-set] (first
                                    result)]
        
        (is
          (= y-row-min
             1)
         )
        
        (is
          (= y-row-max
             7)
         )
        
        (let [[[x-sign-min
                x-sign-max]
               sign-set] (first
                           row-set)]
          (is
            (= x-sign-min
               2)
           )
          
          (is
            (= x-sign-max
               23)
           )
          
          (is
            (= sign-set
               #{[4 19] [4 22] [2 23] [3 20] [3 23]})
           )
          
         )
        
       )
       
      (let [[[y-row-min
              y-row-max] row-set] (first
                                    (rest
                                      result))]
        
        (is
          (= y-row-min
             8)
         )
        
        (is
          (= y-row-max
             10)
         )
        
        (let [[[x-sign-min
                x-sign-max]
               sign-set] (first
                           row-set)]
          (is
            (= x-sign-min
               4)
           )
          
          (is
            (= x-sign-max
               20)
           )
          
          (is
            (= sign-set
               #{[4 19] [5 18]})
           )
          
         )
        
       )
       
      
     )
    
   ))

(deftest test-read-unknown-signs-tasks-fn
  (testing "Test read unknown signs tasks fn"
    
    (let [image-obj nil
          light-value nil
          contrast-value nil
          space-value nil
          hooks-value nil
          n-threads nil
          result (read-unknown-signs-tasks-fn
                   image-obj
                   light-value
                   contrast-value
                   space-value
                   hooks-value
                   n-threads)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          light-value nil
          contrast-value nil
          space-value 16
          hooks-value 8
          n-threads 2
          result (read-unknown-signs-tasks-fn
                   (ImageIO/read
                     (ByteArrayInputStream.
                       image-byte-array))
                   light-value
                   contrast-value
                   space-value
                   hooks-value
                   n-threads)]
      
      (is
        (seq?
          result)
       )
      
      (doseq [result-element result]
        
        (is
          (fn?
            result-element)
         )
        
       )
      
     )
    
   ))

(deftest test-read-unknown-signs-fn
  (testing "Test read unknown signs fn"
    
    (let [image-byte-array nil
          n-threads nil
          light-value nil
          contrast-value nil
          space-value nil
          hooks-value nil
          result (read-unknown-signs-fn
                   image-byte-array
                   n-threads
                   light-value
                   contrast-value
                   space-value
                   hooks-value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          n-threads 2
          light-value nil
          contrast-value nil
          space-value 16
          hooks-value 8
          result (read-unknown-signs-fn
                   image-byte-array
                   n-threads
                   light-value
                   contrast-value
                   space-value
                   hooks-value)]
      
      (is
        (= (count
             result)
           2)
       )
      
      (let [[[y-row-min
              y-row-max]
             row-set] (first
                        result)]
        
        (is
          (= y-row-min
             0)
         )
        
        (is
          (= y-row-max
             21)
         )
        
        (is
          (= (count
               row-set)
             2)
         )
        
       )
      
      (let [[[y-row-min
              y-row-max]
             row-set] (first
                        (rest
                          result))]
        
        (is
          (= y-row-min
             22)
         )
        
        (is
          (= y-row-max
             42)
         )
        
        (is
          (= (count
               row-set)
             1)
         )
        
       )
      
     )
    
   ))

(deftest test-divide-rows
  (testing "Test divide rows"
    
    (let [all-signs nil
          threads nil
          result (divide-rows
                   all-signs
                   threads)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [all-signs nil
          threads 2
          result (divide-rows
                   all-signs
                   threads)]
      
      (is
        (vector?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [all-signs #{[[1 2] "row1"]
                      [[2 3] "row2"]
                      [[3 4] "row3"]
                      [[4 5] "row4"]
                      [[5 6] "row5"]}
          threads 2
          result (divide-rows
                   all-signs
                   threads)]
      
      (is
        (vector?
          result)
       )
      
      (is
        (= result
           [#{[[1 2] "row1"]
              [[3 4] "row3"]
              [[5 6] "row5"]}
            #{[[2 3] "row2"]
              [[4 5] "row4"]}])
       )
      
     )
    
   ))

(deftest test-maching-unknown-signs-fn
  (testing "Test maching unknown signs fn"
    
    (let [all-signs nil
          read-signs nil
          image-byte-array nil
          space-value nil
          matching-value nil
          unknown-sign-count-limit-per-thread nil
          [res-read-text
           res-unknown-signs-images] (maching-unknown-signs-fn
                                       all-signs
                                       read-signs
                                       image-byte-array
                                       space-value
                                       matching-value
                                       unknown-sign-count-limit-per-thread)]
      
      (is
        (= res-read-text
           "")
       )
      
      (is
        (= res-unknown-signs-images
           [])
       )
     
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          image (ImageIO/read
                  (ByteArrayInputStream.
                    image-byte-array))
          sign-i (read-black-dots-fn
                   image)
          all-signs #{[[1 2] #{[[1 2] sign-i]}]}
          read-signs nil
          image-byte-array image-byte-array
          space-value nil
          matching-value nil
          unknown-sign-count-limit-per-thread nil
          result (maching-unknown-signs-fn
                   all-signs
                   read-signs
                   image-byte-array
                   space-value
                   matching-value
                   unknown-sign-count-limit-per-thread)
          [result-text
           result-images-vector] result]
      
      (is
        (= result-text
           "*\n")
       )
      
      (doseq [result-image result-images-vector]
        (is
          (instance?
            BufferedImage
            result-image)
         )
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          image (ImageIO/read
                  (ByteArrayInputStream.
                    image-byte-array))
          sign-i (read-black-dots-fn
                   image)
          all-signs #{[[1 2] #{[[1 2] sign-i]}]}
          read-signs {:i [(bring-to-zero-coordinates-fn
                            sign-i)]}
          image-byte-array image-byte-array
          space-value nil
          matching-value nil
          unknown-sign-count-limit-per-thread nil
          result (maching-unknown-signs-fn
                   all-signs
                   read-signs
                   image-byte-array
                   space-value
                   matching-value
                   unknown-sign-count-limit-per-thread)
          [result-text
           result-images-vector] result]
      
      (is
        (= result-text
           "i\n")
       )
      
      (doseq [result-image result-images-vector]
        (is
          (instance?
            BufferedImage
            result-image)
         )
       )
      
     )
    
   ))

(deftest test-match-unknown-signs-tasks-fn
  (testing "Test match unknown signs tasks fn"
    
    (let [refs nil
          read-signs nil
          image-byte-array nil
          space-value nil
          matching-value nil
          unknown-sign-count-limit-per-thread nil
          result (match-unknown-signs-tasks-fn
                   refs
                   read-signs
                   image-byte-array
                   space-value
                   matching-value
                   unknown-sign-count-limit-per-thread)]
      
      (is
        (seq?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [all-signs #{[[1 2] "row1"]
                      [[2 3] "row2"]
                      [[3 4] "row3"]
                      [[4 5] "row4"]
                      [[5 6] "row5"]}
          threads 2
          refs (divide-rows
                 all-signs
                 threads)
          new-thread-value (count
                             refs)
          image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          image (ImageIO/read
                  (ByteArrayInputStream.
                    image-byte-array))
          sign-i (read-black-dots-fn
                   image)
          read-signs {:i [(bring-to-zero-coordinates-fn
                            sign-i)]}
          space-value nil
          matching-value nil
          unknown-sign-count-limit-per-thread nil
          result (match-unknown-signs-tasks-fn
                   refs
                   read-signs
                   image-byte-array
                   space-value
                   matching-value
                   unknown-sign-count-limit-per-thread)]
      
      (is
        (seq?
          result)
       )
      
      (doseq [result-fn result]
        (is
          (fn?
            result-fn)
         )
       )
      
     )
    
   ))

(deftest test-match-unknown-signs-fn
  (testing "Test match unknown signs fn"
    
    (let [all-signs nil
          read-signs nil
          threads-value nil
          image-byte-array nil
          space-value nil
          matching-value nil
          unknown-sign-count-limit-per-thread nil
          result (match-unknown-signs-fn
                   all-signs
                   read-signs
                   threads-value
                   image-byte-array
                   space-value
                   matching-value
                   unknown-sign-count-limit-per-thread)]
      
      (is
        (= result
           ["" []])
       )
      
     )
    
    (let [image-path "resources/dostojevski/i.jpg"
          image-is (FileInputStream.
                     (File.
                       image-path))
          available-bytes (.available
                            image-is)
          image-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-is
                    image-byte-array)
          image (ImageIO/read
                  (ByteArrayInputStream.
                    image-byte-array))
          sign-i (read-black-dots-fn
                   image)
          sign-i-zero-coordinates (bring-to-zero-coordinates-fn
                                    sign-i)
          all-signs (sorted-set-by
                      sort-rows
                      [[0 25] #{[[1 2] sign-i-zero-coordinates]}])
          all-signs (conj
                      all-signs
                      [[30 55] #{[[1 2] #{[1 2]}]}]
                      [[60 85] #{[[1 2] sign-i-zero-coordinates]}]
                      [[90 115] #{[[1 2] sign-i-zero-coordinates]}]
                      [[120 145] #{[[1 2] #{[2 3]}]}])
          read-signs {:i [sign-i-zero-coordinates]}
          threads-value 2
          image-byte-array image-byte-array
          space-value nil
          matching-value nil
          unknown-sign-count-limit-per-thread nil
          result (match-unknown-signs-fn
                   all-signs
                   read-signs
                   threads-value
                   image-byte-array
                   space-value
                   matching-value
                   unknown-sign-count-limit-per-thread)
          [result-text
           result-images-vector] result]
      
      (is
        (= result-text
           "i\n*\ni\ni\n*\n")
       )
      
      (doseq [result-image result-images-vector]
        (is
          (instance?
            BufferedImage
            result-image)
         )
       )
      
     )
    
   ))

(deftest test-read-image-fn
  (testing "Test read image fn"
    
    (let [image-byte-array nil
          light-value nil
          contrast-value nil
          space-value nil
          hooks-value nil
          matching-value nil
          threads-value nil
          rows-threads-value nil
          signs nil
          unknown-sign-count-limit-per-thread nil
          result (read-image-fn
                   image-byte-array
                   light-value
                   contrast-value
                   space-value
                   hooks-value
                   matching-value
                   threads-value
                   rows-threads-value
                   signs
                   unknown-sign-count-limit-per-thread)]
      
      (is
        (= result
           ["" []])
       )
      
     )
    
    (let [image-t-path "resources/dostojevski/tekst2.jpg"
          image-t-is (FileInputStream.
                       (File.
                         image-t-path))
          available-bytes (.available
                            image-t-is)
          image-t-byte-array (byte-array
                               available-bytes)
          read-is (.read
                    image-t-is
                    image-t-byte-array)
          image-t-byte-array image-t-byte-array
          image-i-path "resources/dostojevski/i.jpg"
          image-i-is (FileInputStream.
                       (File.
                         image-i-path))
          available-bytes (.available
                            image-i-is)
          image-i-byte-array (byte-array
                             available-bytes)
          read-is (.read
                    image-i-is
                    image-i-byte-array)
          image-i-byte-array image-i-byte-array
          image-i (ImageIO/read
                    (ByteArrayInputStream.
                      image-i-byte-array))
          image-i-black-dots (read-black-dots-fn
                               image-i)
          image-i-zero-coordinates (bring-to-zero-coordinates-fn
                                     image-i-black-dots)
          [_ _ x-max y-max] (find-min-max-fn
                              image-i-zero-coordinates)
          width (inc
                  x-max)
          height (inc
                   y-max)
          new-image-i (.getSubimage
                        (ImageIO/read
                          (ByteArrayInputStream.
                            image-i-byte-array))
                        0
                        0
                        width
                        height)
          void (draw-sign
                 new-image-i
                 image-i-zero-coordinates)
          image-i-os (ByteArrayOutputStream.)
          void (ImageIO/write
                 new-image-i
                 "jpg"
                 image-i-os)
          new-image-i-byte-array (.toByteArray
                                   image-i-os)
          light-value nil
          contrast-value nil
          space-value nil
          hooks-value 8
          matching-value 50
          threads-value 2
          rows-threads-value 2
          signs {:i [new-image-i-byte-array]}
          unknown-sign-count-limit-per-thread nil
          result (read-image-fn
                   image-t-byte-array
                   light-value
                   contrast-value
                   space-value
                   hooks-value
                   matching-value
                   threads-value
                   rows-threads-value
                   signs
                   unknown-sign-count-limit-per-thread)
          [result-text
           result-images-vector] result]
      
      (is
        (= result-text
           "* **** i ********\n")
       )
      
      (doseq [result-image result-images-vector]
        (is
          (instance?
            BufferedImage
            result-image)
         )
       )
      
     )
    
   ))

