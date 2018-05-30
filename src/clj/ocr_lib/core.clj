(ns ocr-lib.core
 (:require [clojure.set :refer [intersection difference union select]])
 (:import [java.io IOException]
          [java.awt.image BufferedImage]
          [javax.imageio ImageIO]
          [java.io ByteArrayInputStream]))

(defn contrast-fn
 "Return value between 0 - 255 depending on contrast-value parameter
  if current value (color parameter) is number less then 128
  value should strive for 0 or black color
  if current value (color parameter) is number greater then 127
  value should strive for 255 or white color"
 [color
  contrast-value]
 (if (< color 128)
  (let [color (- color contrast-value)]
   (if (neg? color)
    0
    color))
  (let [color (+ color contrast-value)]
   (if (< 255 color)
    255
    color))
  ))

(defn grayscale-contrast-fn
 "Put image in gray scale mode
  and apply light and contrast values"
 [image
  width
  height
  & [light-value
     contrast-value]]
 (doseq [x (range width)]
  (doseq [y (range height)]
   (let [p (.getRGB image x y)
         a (bit-and (bit-shift-right p 24) ; transparency
                    127)
         r (bit-and (bit-shift-right p 16) ; red
                    255)
         g (bit-and (bit-shift-right p 8) ; green
                    255)
         b (bit-and (bit-shift-right p 0) ; blue
                    255)
         ;r (- 255 r) negative
         ;g (- 255 g) negative
         ;b (- 255 b) negative
         avg (int (/ (+ r g b (* (or light-value 0) 3)) 3))
         contrast-value (or contrast-value 128)
         r (contrast-fn avg contrast-value)
         g (contrast-fn avg contrast-value)
         b (contrast-fn avg contrast-value)
         a (bit-shift-left 0 24) ; transparency
         r (bit-shift-left r 16) ; red
         g (bit-shift-left g 8) ; green
         b (bit-shift-left b 0) ; blue
         p (bit-or a r g b)]
    (.setRGB image x y p)
    ))
  )
  image)

(defn dot-is-black?
 "Check if dot with particular coordinates is black"
 [image
  x
  y]
 (when (and (not (neg? x))
            (not (neg? y))
            (< x (.getWidth image))
            (< y (.getHeight image))
        )
  (let [p (.getRGB image x y)
        a (bit-and (bit-shift-right p 24) ; transparency
                   127)
        r (bit-and (bit-shift-right p 16) ; red
                   255)
        g (bit-and (bit-shift-right p 8) ; green
                   255)
        b (bit-and (bit-shift-right p 0) ; blue
                   255)]
   (and (= r 0)
        (= g 0)
        (= b 0))
   ))
 )

(defn read-black-dots-fn
 "Return coordinates set which are black dots in particular image"
 [image
  width
  height]
 (let [all-dots (atom #{})]
  (doseq [x (range 0 width)]
   (doseq [y (range 0 height)]
    (when (dot-is-black? image x y)
     (swap! all-dots conj [x y])
     ))
   )
  @all-dots))

(defn neighbour-dots-set-fn
 "Return eight pairs of coordinates which are surrounding passed pair of coordinates"
 [x y]
 #{[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]
   [(dec x) (inc y)]
   [(inc x) (inc y)]
   [(inc x) (dec y)]
   [(dec x) (dec y)]})

(defn find-sign-dots-fn
 "Find coordinates that correspond to dots of a single sign"
 [all-dots
  sign-dots
  checked-dots
  width
  height]
 (if (empty? (difference
               sign-dots
               checked-dots))
  [all-dots sign-dots]
  (let [not-checked-dots (difference
                           sign-dots
                           checked-dots)
        [x y] (first not-checked-dots)
        neighbour-dots (neighbour-dots-set-fn x y)
        neighbour-dots (reduce
                        (fn
                         [acc
                          [x y]]
                         (if (and (< x width)
                                  (> x -1)
                                  (< y height)
                                  (> y -1)
                                  (contains? all-dots [x y]))
                          (conj acc [x y])
                          acc))
                         #{}
                         neighbour-dots)
        all-dots (apply disj all-dots neighbour-dots)
        sign-dots (apply conj sign-dots neighbour-dots)
        checked-dots (conj checked-dots [x y])]
   (recur all-dots sign-dots checked-dots width height))
  ))

(defn find-min-max-fn
 "Find x's and y's minimum and maximum
  from dots-set parameter"
 [dots-set]
 (let [x-min (atom (Integer/MAX_VALUE))
       y-min (atom (Integer/MAX_VALUE))
       x-max (atom 0)
       y-max (atom 0)]
  (doseq [[x y] dots-set]
   (when (< x @x-min)
    (reset! x-min x))
   (when (< y @y-min)
    (reset! y-min y))
   (when (< @x-max x)
    (reset! x-max x))
   (when (< @y-max y)
    (reset! y-max y))
   )
;  (println [@x-min @y-min])
  [@x-min @y-min
   @x-max @y-max]))

(defn find-dots-hooks-fn
 "Find dots, hooks, dashes...
  above or below the sign"
 [all-dots
  x-min
  x-max
  y-min
  y-max
  hooks-value]
 (let [examining-area (atom #{})]
  (doseq [x (range x-min x-max)]
   (doseq [y (range (- y-min hooks-value) (dec y-min))]
    (swap! examining-area conj [x y]))
   )
  (doseq [x (range x-min x-max)]
   (doseq [y (range (inc y-max) (+ y-max hooks-value))]
    (swap! examining-area conj [x y]))
   )
  (intersection all-dots @examining-area))
 )

(defn sort-row-elements
 "Function that sorts set elements
  (set is representing row)"
 [[[x-min1 _] elem1]
  [[x-min2 _] elem2]]
  (< x-min1 x-min2))

(defn add-into-sorted-set-fn
 "Add sign into sorted set"
 [dots-sets-of-signs
  x-min
  y-min
  x-max
  y-max
  sign-dots]
 (let [row (select
            (fn [[[y-min-p y-max-p] elem-p]]
             (let [y-row-set (set (range y-min-p y-max-p))
                   y-sign-set (set (range y-min y-max))]
              (not
               (empty?
                (intersection
                  y-row-set
                  y-sign-set))
               ))
             )
             dots-sets-of-signs)
       row-element (sorted-set-by
                     sort-row-elements)]
  (if (empty? row)
   (conj dots-sets-of-signs
         [[y-min y-max] (conj row-element
                              [[x-min x-max] sign-dots])])
   (let [dots-sets-of-signs (difference
                              dots-sets-of-signs
                              row)
         first-row (first row)
         [[y-min-r y-max-r] elem-r] first-row
         y-min-d (atom
                  (if (< y-min
                         y-min-r)
                   y-min
                   y-min-r))
         y-max-d (atom
                  (if (< y-max
                         y-max-r)
                   y-max-r
                   y-max))
         new-row (atom
                  (sorted-set-by
                    sort-row-elements))]
    (doseq [[[y-min-s y-max-s] elem-s] row]
     (when (< y-min-s @y-min-d)
      (reset! y-min-d y-min-s))
     (when (< @y-max-d y-min-s)
      (reset! y-max-d y-max-s))
     (doseq [el-s elem-s]
      (swap! new-row conj el-s))
     )
    (swap! new-row conj [[x-min x-max] sign-dots])
    (conj dots-sets-of-signs
          [[@y-min-d @y-max-d] @new-row]))
   ))
 )

(defn grouping-dots-fn
 "Grouping dots into sets which will represent separated signs"
 [all-dots
  dots-sets-of-signs
  width
  height
  hooks-value]
 (if (empty? all-dots)
  dots-sets-of-signs
  (let [[x y] (first all-dots)
        all-dots (disj all-dots [x y])
        [all-dots sign-dots] (find-sign-dots-fn
                               all-dots
                               #{[x y]}
                               #{}
                               width
                               height)
        [x-min y-min x-max y-max] (find-min-max-fn
                                    sign-dots)
        dots-hooks-set (find-dots-hooks-fn
                         all-dots
                         x-min
                         x-max
                         y-min
                         y-max
                         hooks-value)
        [all-dots sign-dots] (if (empty? dots-hooks-set)
                                      [all-dots sign-dots]
                                      (find-sign-dots-fn
                                        all-dots
                                        (union sign-dots
                                               dots-hooks-set)
                                        sign-dots
                                        width
                                        height))
        [x-min y-min x-max y-max] (find-min-max-fn
                                    sign-dots)        
        dots-sets-of-signs (add-into-sorted-set-fn
                             dots-sets-of-signs
                             x-min
                             y-min
                             x-max
                             y-max
                             sign-dots)]
    ;(println (count all-dots))
    (recur all-dots dots-sets-of-signs width height hooks-value))
  ))

(defn read-signs-fn
 "Read known signs images from byte arrays"
 [signs
  light-value
  contrast-value]
 (let [signs-dots (atom {})
       signs-keys (keys signs)]
  (doseq [s-key signs-keys]
   (let [images-sign-vector (s-key signs)
         s-key-value (atom [])]
    (doseq [image-byte-array images-sign-vector]
     (let [image (ImageIO/read (ByteArrayInputStream. image-byte-array))
           width (.getWidth image)
           height (.getHeight image)
           image (grayscale-contrast-fn
                   image
                   width
                   height
                   light-value
                   contrast-value)
           all-dots (read-black-dots-fn image width height)]
      (swap!
        s-key-value
        conj
        all-dots))
     )
     (swap!
       signs-dots
       assoc
       s-key
       @s-key-value))
   )
  @signs-dots))

(defn bring-to-zero-coordinates-fn
 "Bring coordinates from dots-set parameter to minimal positive values"
 [dots-set]
 (let [[x-min y-min x-max y-max] (find-min-max-fn dots-set)
       new-dots-set (atom #{})]
  (doseq [[x y] dots-set]
   (swap! new-dots-set conj [(- x x-min) (- y y-min)]))
  @new-dots-set))

(defn check-matching-fn
 "Check matching of zero coordinates unknown sign
  with all known signs and return sign and matching percentage"
 [zero-coordinates
  read-signs]
 (let [result (atom {:sign ""
                     :matching 0})
       signs-keys (keys read-signs)]
  (doseq [s-key signs-keys]
   (doseq [dots-set (s-key read-signs)]
    (let [intersection-count (count (intersection zero-coordinates dots-set))
          union-count (count (union zero-coordinates dots-set))
          matching-percentage (double
                             (/ (* intersection-count
                                   100)
                                union-count))
          matching (:matching @result)]
     (when (< matching
              matching-percentage)
      (swap!
        result
        assoc
        :sign (name s-key)
        :matching matching-percentage))
     ))
   )
  @result))

(defn draw-sign
 "Draw image in image parameter"
 [image
  coordinates-set]
 (let [width (.getWidth image)
       height (.getHeight image)]
  (doseq [x (range width)]
   (doseq [y (range height)]
    (let [a (bit-shift-left 127 24) ; transparency
          r (bit-shift-left 255 16) ; red
          g (bit-shift-left 255 8) ; green
          b (bit-shift-left 255 0) ; blue
          p (bit-or a r g b)]
     (.setRGB image x y p))
    ))
  )
 (doseq [[x y] coordinates-set]
  (let [a (bit-shift-left 127 24) ; transparency
        r (bit-shift-left 0 16) ; red
        g (bit-shift-left 0 8) ; green
        b (bit-shift-left 0 0) ; blue
        p (bit-or a r g b)]
   (.setRGB image x y p))
  )
 image)

(defn read-image-fn
 "Read text from image"
 [image-byte-array
  light-value
  contrast-value
  space-value
  hooks-value
  matching-value
  signs]
 (let [image (ImageIO/read (ByteArrayInputStream. image-byte-array))
       width (.getWidth image)
       height (.getHeight image)
       image (grayscale-contrast-fn
               image
               width
               height
               light-value
               contrast-value)
       all-dots (read-black-dots-fn image width height)
       all-signs (grouping-dots-fn
                   all-dots
                   (sorted-set-by
                    (fn
                     [[[y-min1 y-max1] elem1]
                      [[y-min2 y-max2] elem2]]
                     (< y-max1 y-min2))
                    )
                   width
                   height
                   hooks-value)
       signs-images (atom [])
       read-signs (read-signs-fn
                    signs
                    light-value
                    contrast-value)
       ]
  (doseq [[[y-min y-max] row] all-signs]
   (let [previous-sign-x-max (atom (Integer/MAX_VALUE))]
    (doseq [[[x-min x-max] sign-dots] row]
     (try
      (let [zero-coordinates (bring-to-zero-coordinates-fn sign-dots)
            {sign :sign
             matching :matching} (check-matching-fn
                                   zero-coordinates
                                   read-signs)
            [_ _ x-max y-max] (find-min-max-fn zero-coordinates)
            width (inc x-max)
            height (inc y-max)
            image (.getSubimage
                 (ImageIO/read
                  (ByteArrayInputStream. image-byte-array))
                 0
                 0
                 width
                 height)
            space-area (- x-min
                          @previous-sign-x-max)]
       (when (> space-area
                space-value)
        (swap!
         signs-images
         conj
         " "))
       (if (> matching
              matching-value)
        (swap!
         signs-images
         conj
         sign)
        (do
         (draw-sign image zero-coordinates)
         (swap!
           signs-images
           conj
           image))
        ))
      (catch IOException e
       (println "Error")
       (println (.getMessage e))
       ))
      (reset!
        previous-sign-x-max
        x-max)
     ))
   (swap!
     signs-images
     conj
     "\n")
   )
  @signs-images))

