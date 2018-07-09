(ns ocr-lib.core
  (:require [clojure.set :refer [intersection difference union select]]
            [utils-lib.core :as utils])
  (:import [java.util Base64]
           [java.io IOException]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io ByteArrayInputStream]
           [java.util.concurrent Executors]))

(def base64-decoder
     (Base64/Decoder/getDecoder))

(def process-images-progress-bar-value
     (atom 0))

(def total-pixels-value
     (atom 0))

(defn process-images-calculate-progress-value-fn
  "Calculate progress value for process images function"
  []
  (if (> @total-pixels-value
         0)
    (int
      (/ (* @process-images-progress-bar-value
            100)
         @total-pixels-value))
    0))

(defn process-images-reset-progress-value-fn
  "Reset progress values"
  []
  (reset!
    total-pixels-value
    0)
  (reset!
    process-images-progress-bar-value
    0))

(defn increase-total-pixels
  "Increase total pixels for progress value"
  [width
   height]
  (swap!
    total-pixels-value
    +
    (* width
       height))
 )

(def grayscale-contrast-progress-value
     (atom 0))

(def read-black-dots-progress-value
     (atom 0))

(def grouping-dots-progress-value
     (atom 0))

(def grouping-all-dots
     (atom 0))

(def grouping-processed-dots
     (atom 0))

(def read-signs-progress-value
     (atom 0))

(def matching-progress-value
     (atom 0))

(defn read-image-calculate-progress-value-fn
  "Not implemented"
  []
  (let [grayscale-contrast (if (> @total-pixels-value
                                  0)
                             (int
                              (/ (* @process-images-progress-bar-value
                                    100)
                                 @total-pixels-value))
                             0)
        read-black-dots 0
        grouping-dots (if (> @grouping-all-dots
                             0)
                        (int
                          (/ (* @grouping-processed-dots
                                100)
                             @grouping-all-dots))
                        0)
        read-signs 0
        matching 0]
    (when (< @grayscale-contrast-progress-value
             grayscale-contrast)
      (reset!
        grayscale-contrast-progress-value
        grayscale-contrast))
    (when (< @read-black-dots-progress-value
             read-black-dots)
      (reset!
        read-black-dots-progress-value
        read-black-dots))
    (when (< @grouping-dots-progress-value
             grouping-dots)
      (reset!
        grouping-dots-progress-value
        grouping-dots))
    (when (< @read-signs-progress-value
             read-signs)
      (reset!
        read-signs-progress-value
        read-signs))
    (when (< @matching-progress-value
             matching)
      (reset!
        matching-progress-value
        matching))
    (int
      (+ (* @grayscale-contrast-progress-value
            0.07)
         (* @read-black-dots-progress-value
            0.09)
         (* @grouping-dots-progress-value
            0.46)
         (* @read-signs-progress-value
            0.02)
         (* @matching-progress-value
            0.36))
     ))
 )

(defn read-image-reset-progress-value-fn
  "Reset values for read image progress"
  []
  (reset!
    grayscale-contrast-progress-value
    0)
  (reset!
    read-black-dots-progress-value
    0)
  (reset!
    grouping-dots-progress-value
    0)
  (reset!
    grouping-all-dots
    0)
  (reset!
    read-signs-progress-value
    0)
  (reset!
    matching-progress-value
    0)
  (process-images-reset-progress-value-fn))

(defn read-base64-image-fn
  "Decodes base64 image into byte array and read it into BufferedImage"
  [image-base64]
  (let [image-byte-array (.decode
                           base64-decoder
                           image-base64)
        image-is (ByteArrayInputStream.
                   image-byte-array)
        image (ImageIO/read
                image-is)]
    image))

(defn contrast-fn
  "Return value between 0 - 255 depending on contrast-value parameter
   if current value (color parameter) is number less then 128
   value should strive for 0 or black color
   if current value (color parameter) is number greater then 127
   value should strive for 255 or white color"
  [color
   contrast-value]
  (if (< color
         128)
    (let [color (- color
                   contrast-value)]
      (if (neg? color)
        0
        color))
    (let [color (+ color
                   contrast-value)]
      (if (< 255
             color)
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
      (swap!
        process-images-progress-bar-value
        inc)
      (let [p (.getRGB
                image
                x
                y)
            a (bit-and ; transparency
                (bit-shift-right
                  p
                  24)
                127)
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
                255)
            ;r (- 255 r) negative
            ;g (- 255 g) negative
            ;b (- 255 b) negative
            avg (int
                  (/ (+ r
                        g
                        b
                        (* (or light-value
                               0)
                           3))
                     3))
            contrast-value (or contrast-value
                               128)
            r (contrast-fn
                avg
                contrast-value)
            g (contrast-fn
                avg
                contrast-value)
            b (contrast-fn
                avg
                contrast-value)
            a (bit-shift-left ; transparency
                0
                24)
            r (bit-shift-left ; red
                r
                16)
            g (bit-shift-left ; green
                g
                8)
            b (bit-shift-left ; blue
                b
                0)
            p (bit-or
                a
                r
                g
                b)]
       (.setRGB
         image
         x
         y
         p))
     ))
  image)

(defn read-black-dots-fn
  "Read coordinates of black dots of image"
  [image
   width
   height
   & [light-value
      contrast-value]]
  (let [all-dots (atom #{})]
    (doseq [x (range width)]
      (doseq [y (range height)]
        (swap!
          process-images-progress-bar-value
          inc)
        (let [p (.getRGB
                  image
                  x
                  y)
              a (bit-and
                  (bit-shift-right ; transparency
                    p
                    24)
                  127)
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
                  255)
              avg (int
                    (/ (+ r
                          g
                          b
                          (* (or light-value
                                 0)
                             3))
                       3))
              contrast-value (or contrast-value
                                 128)]
          (when (= (contrast-fn
                     avg
                     contrast-value)
                   0)
            (swap!
              all-dots
              conj
              [x y]))
         ))
     )
    @all-dots))

(defn read-black-dots-part-fn
  "Read coordinates of black dots of image part"
  [image
   width
   height
   start-y
   end-y
   & [light-value
      contrast-value]]
  (let [all-dots (atom #{})
        condition-i (> (dec start-y)
                       -1)
        start-y-dec (if condition-i
                      (dec start-y)
                      start-y)
        condition-ii (< (inc end-y)
                        height)
        end-y-inc (if condition-ii
                    (inc end-y)
                    end-y)
        white-lines (atom (sorted-set))]
    (doseq [x (range width)]
      (doseq [y (range start-y-dec end-y-inc)]
        (let [p (.getRGB
                  image
                  x
                  y)
              a (bit-and ; transparency
                  (bit-shift-right
                    p
                    24)
                  127)
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
                  255)
              avg (int
                    (/ (+ r
                          g
                          b
                          (* (or light-value
                                 0)
                              3))
                        3))
              contrast-value (or contrast-value
                                 128)]
          (when (and
                  (not
                    (or (and (= start-y-dec
                                y)
                             condition-i)
                        (and (= (dec end-y-inc)
                                y)
                             condition-ii))
                   )
                  (= (contrast-fn
                       avg
                       contrast-value)
                     0))
            (swap!
              all-dots
              conj
              [x y]))
         ))
     )
    @all-dots))

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
   (let [p (.getRGB
             image
             x
             y)
         a (bit-and ; transparency
             (bit-shift-right
               p
               24)
             127)
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
    (and (= r 0)
         (= g 0)
         (= b 0))
    ))
 )

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
    [all-dots
     sign-dots]
    (let [not-checked-dots (difference
                             sign-dots
                             checked-dots)
          [x y] (first
                  not-checked-dots)
          neighbour-dots (neighbour-dots-set-fn
                           x
                           y)
          neighbour-dots (reduce
                           (fn [acc
                                [x y]]
                             (if (and (< x width)
                                      (> x -1)
                                      (< y height)
                                      (> y -1)
                                      (contains? all-dots
                                                 [x y]))
                               (conj
                                 acc
                                 [x y])
                               acc))
                           #{}
                           neighbour-dots)
          all-dots (apply
                     disj
                     all-dots
                     neighbour-dots)
          sign-dots (apply
                      conj
                      sign-dots
                      neighbour-dots)
          checked-dots (conj
                         checked-dots
                         [x y])]
      (recur
        all-dots
        sign-dots
        checked-dots
        width
        height))
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
     (when (< x
              @x-min)
       (reset!
         x-min
         x))
     (when (< y
              @y-min)
       (reset!
         y-min
         y))
     (when (< @x-max
              x)
       (reset!
         x-max
         x))
     (when (< @y-max
              y)
       (reset!
         y-max
         y))
    )
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
    (doseq [x (range x-min
                     x-max)]
      (doseq [y (range (- y-min
                          hooks-value)
                       (dec y-min))]
        (swap!
          examining-area
          conj
          [x y]))
     )
    (doseq [x (range x-min
                     x-max)]
      (doseq [y (range (inc y-max)
                       (+ y-max
                          hooks-value))]
        (swap!
          examining-area
          conj
          [x y]))
     )
    (intersection
      all-dots
      @examining-area))
 )

(defn sort-row-elements
  "Function that sorts set elements
   (set is representing row with signs)"
  [[[x-min1 x-max1] elem1]
   [[x-min2 x-max2] elem2]]
   (or (< x-min1 x-min2)
       (< x-max1 x-max2))
 )

(defn sort-rows
  "Function that sorts set elements
   (set is representing page with rows)"
  [[[y-min1 y-max1] elem1]
   [[y-min2 y-max2] elem2]]
  (and (< y-max1 y-min2)
       (< y-min1 y-max2))
 )

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
                (or (<= y-min-p y-min y-max-p)
                    (<= y-min-p y-max y-max-p)
                    (<= y-min y-min-p y-max)
                    (<= y-min y-max-p y-max))
               )
              dots-sets-of-signs)
        row-element (sorted-set-by
                      sort-row-elements)]
   (if (empty? row)
     (conj
       dots-sets-of-signs
       [[y-min y-max]
        (conj
          row-element
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
          (reset!
            y-min-d
            y-min-s))
        (when (< @y-max-d y-min-s)
          (reset!
            y-max-d
            y-max-s))
        (doseq [el-s elem-s]
          (swap!
            new-row
            conj
            el-s))
       )
      (swap!
        new-row
        conj
        [[x-min x-max] sign-dots])
      (conj
        dots-sets-of-signs
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
          all-dots (disj
                     all-dots
                     [x y])
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
                                 [all-dots
                                  sign-dots]
                                 (find-sign-dots-fn
                                   all-dots
                                   (union
                                     sign-dots
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
      (recur
        all-dots
        dots-sets-of-signs
        width
        height
        hooks-value))
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
          (let [image (ImageIO/read
                        (ByteArrayInputStream.
                          image-byte-array))
                width (.getWidth image)
                height (.getHeight image)
                all-dots (read-black-dots-fn
                           image
                           width
                           height
                           light-value
                           contrast-value)]
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
  (let [[x-min
         y-min
         x-max
         y-max] (find-min-max-fn
                  dots-set)
        new-dots-set (atom #{})]
    (doseq [[x y] dots-set]
      (swap!
        new-dots-set
        conj
        [(- x x-min) (- y y-min)]))
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
        (let [intersection-count (count
                                   (intersection
                                     zero-coordinates
                                     dots-set))
              union-count (count
                            (union
                              zero-coordinates
                              dots-set))
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
        (let [a (bit-shift-left ; transparency
                  127
                  24)
              r (bit-shift-left ; red
                  255
                  16)
              g (bit-shift-left ; green
                  255
                  8)
              b (bit-shift-left ; blue
                  255
                  0)
              p (bit-or
                  a
                  r
                  g
                  b)]
         (.setRGB
           image
           x
           y
           p))
       ))
   )
  (doseq [[x y] coordinates-set]
    (let [a (bit-shift-left ; transparency
              127
              24)
          r (bit-shift-left ; red
              0
              16)
          g (bit-shift-left ; green
              0
              8)
          b (bit-shift-left ; blue
              0
              0)
          p (bit-or
              a
              r
              g
              b)]
     (.setRGB
       image
       x
       y
       p))
   )
  image)

(defn grouping-dots-one-part-fn
  "Grouping dots from one part of image"
  [image
   thread-number
   n-threads
   light-value
   contrast-value
   space-value
   hooks-value
   matching-value]
  (let [width (.getWidth image)
        height (.getHeight image)
        one-part-height (int
                          (/ height
                             n-threads))
        last-part-height (if (= n-threads
                                (inc thread-number))
                          (+ one-part-height
                             (dec
                               (mod height
                                    n-threads))
                           )
                          one-part-height)
        start-y (* thread-number
                   one-part-height)
        end-y (+ start-y
                 last-part-height)
        all-dots (read-black-dots-part-fn
                   image
                   width
                   height
                   start-y
                   end-y
                   light-value
                   contrast-value)
        all-signs (grouping-dots-fn
                    all-dots
                    (sorted-set-by
                      sort-rows)
                    width
                    height
                    hooks-value)]
    all-signs))

(defn merge-and-disj
  "Merge sign parts from selected set and remove them from row that contains them"
  [selected-signs-set
   new-x-min
   new-x-max
   new-sign
   row]
  (doseq [[[x-min x-max] sign] selected-signs-set]
    (when (< x-min
             @new-x-min)
      (reset!
        new-x-min
        x-min))
    (when (< @new-x-max
             x-max)
      (reset!
        new-x-max
        x-max))
    (swap!
      new-sign
      union
      sign)
    (swap!
      row
      disj
      [[x-min x-max] sign]))
 )

(defn merge-sign-coordinates-concrete
  "Merge sign parts from both row parts"
  [row-f
   row-s
   new-sign
   new-x-min
   new-x-max]
  (let [select-pred? (fn [[[x-min x-max] sign]]
                       (or (<= x-min @new-x-min x-max)
                           (<= x-min @new-x-max x-max)
                           (<= @new-x-min x-min @new-x-max)
                           (<= @new-x-min x-max @new-x-max))
                      )
        selected-signs-set-f (select
                               select-pred?
                               @row-f)
        selected-signs-set-s (select
                               select-pred?
                               @row-s)]
    (if (and (empty? selected-signs-set-f)
             (empty? selected-signs-set-s))
      [[@new-x-min
        @new-x-max]
       @new-sign]
      (do
        (merge-and-disj
          selected-signs-set-f
          new-x-min
          new-x-max
          new-sign
          row-f)
        (merge-and-disj
          selected-signs-set-s
          new-x-min
          new-x-max
          new-sign
          row-s)
        (recur
          row-f
          row-s
          new-sign
          new-x-min
          new-x-max))
     ))
 )

(defn merge-sign-coordinates
  "Merge sign parts, sign by sign, and return merged row"
  [row-f
   row-s
   result
   new-sign
   new-x-min
   new-x-max]
  (let [condition-i (empty? @row-f)
        condition-ii (empty? @row-s)
        condition-iii (and (not condition-i)
                           condition-ii)
        condition-iv (and (not condition-ii)
                          condition-i)
        condition-v (and condition-i
                         condition-ii)
        condition-vi (and (not condition-i)
                          (not condition-ii))
        condition-vii (empty? @new-sign)
        swap-apply-fn (fn [param1 param-fn param2] (apply param-fn param1 param2))]
    (if condition-v
      @result
      (if condition-iii
        (do
          (swap!
            result
            swap-apply-fn
            conj
            @row-f)
          (swap!
            row-f
            empty)
          (recur
            row-f
            row-s
            result
            new-sign
            new-x-min
            new-x-max))
        (if condition-iv
          (do
            (swap!
              result
              swap-apply-fn
              conj
              @row-s)
            (swap!
              row-s
              empty)
            (recur
              row-f
              row-s
              result
              new-sign
              new-x-min
              new-x-max))
           (let [[[x-min-f x-max-f] sign-f] (first @row-f)
                 selected-signs-set-s (select
                                        (fn [[[x-min x-max] sign]]
                                          (or (<= x-min x-min-f x-max)
                                              (<= x-min x-max-f x-max)
                                              (<= x-min-f x-min x-max-f)
                                              (<= x-min-f x-max x-max-f))
                                         )
                                        @row-s)]
             (reset!
               new-x-min
               x-min-f)
             (reset!
               new-x-max
               x-max-f)
             (reset!
               new-sign
               sign-f)
             (swap!
               row-f
               disj
               [[x-min-f x-max-f] sign-f])
             (swap!
               result
               conj
               (merge-sign-coordinates-concrete
                 row-f
                 row-s
                 new-sign
                 new-x-min
                 new-x-max))
             (reset!
               new-x-min
               (Integer/MAX_VALUE))
             (reset!
               new-x-max
               0)
             (reset!
               new-sign
               #{})
             (recur
               row-f
               row-s
               result
               new-sign
               new-x-min
               new-x-max))
            ))
     ))
 )

(defn merge-separated-parts
  "Merge parts separated by partial image processing"
  [sorted-rows-set]
  (let [{[[y-min-p y-max-p] row-p] :previous
         result :result} (reduce
                           (fn [{result :result
                                 previous :previous}
                                [[y-min-c
                                  y-max-c]
                                 row-c]]
                             (when-let [[[y-min-p y-max-p] row-p] previous]
                               (if (= y-min-c
                                      (inc y-max-p))
                                 (swap!
                                   result
                                   conj
                                   [[y-min-p
                                     y-max-c]
                                    (merge-sign-coordinates
                                      (atom row-p)
                                      (atom row-c)
                                      (atom
                                        (sorted-set-by
                                          sort-row-elements))
                                      (atom #{})
                                      (atom (Integer/MAX_VALUE))
                                      (atom 0))])
                                 (swap!
                                   result
                                   conj
                                   [[y-min-p y-max-p] row-p]))
                              )
                             {:result result
                              :previous [[y-min-c
                                          y-max-c]
                                         row-c]}
                             )
                           {:result (atom
                                      (sorted-set-by
                                        sort-rows))
                            :previous nil}
                           sorted-rows-set)
        [[y-min-l y-max-l] row-l] (last @result)
        previous-merged (or (<= y-min-p y-min-l y-max-p)
                            (<= y-min-p y-max-l y-max-p)
                            (<= y-min-l y-min-p y-max-l)
                            (<= y-min-l y-max-p y-max-l))
        final-result (if previous-merged
                       @result
                       (conj
                         @result
                         [[y-min-p y-max-p] row-p]))]
    final-result))

(defn read-unknown-signs-tasks-fn
  "Read unknown signs from image, n-threads parts in parallel"
  [refs
   result-refs]
  (map
    (fn [[image
          light-value
          contrast-value
          space-value
          hooks-value
          matching-value]
         thread-number
         n-treads]
      (fn []
       (dosync
        (let [all-signs (grouping-dots-one-part-fn
                          image
                          thread-number
                          n-treads
                          light-value
                          contrast-value
                          space-value
                          hooks-value
                          matching-value)]
          (swap!
            result-refs
            union
            all-signs)
         ))
       )
     )
    refs
    (range (count refs))
    (repeat (count refs) (count refs))
   ))

(defn read-unknown-signs-fn
  "Read unknown signs from image
   
   Multiple threads will be running when processing this function"
  [image-byte-array
   n-threads
   light-value
   contrast-value
   space-value
   hooks-value
   matching-value]
  (let [refs (repeat
               n-threads
               [(ImageIO/read
                  (ByteArrayInputStream.
                    image-byte-array))
                light-value
                contrast-value
                space-value
                hooks-value
                matching-value])
        result-refs (atom
                      (sorted-set-by
                        sort-rows))
        pool (Executors/newFixedThreadPool
               n-threads)
        tasks (read-unknown-signs-tasks-fn
                refs
                result-refs)]
    (doseq [future (.invokeAll
                     pool
                     tasks)]
      (.get future))
    (.shutdown
      pool)
    (merge-separated-parts
      @result-refs))
 )

(defn divide-rows
 "Divide rows into separate sets for multi processing"
 [all-signs
  threads]
 (let [rows (count all-signs)
       rows-per-thread (int
                         (/ rows
                            threads))
       mod-threads (mod rows
                        threads)
       rows-per-thread-seq (atom
                             (into
                               []
                               (repeat
                                 threads
                                 (atom
                                   rows-per-thread))
                              ))
       result (atom [])
       one-thread (atom
                    (sorted-set-by
                      sort-rows))]
    (doseq [itr (range mod-threads)]
      (let [rows-per-thread-doseq (get @rows-per-thread-seq itr)]
        (swap!
          rows-per-thread-seq
          utils/replace-in-vector-on-index
          (atom (inc @rows-per-thread-doseq))
          itr))
     )
    (doseq [row all-signs]
      (let [count-result (count @result)
            count-atom (get @rows-per-thread-seq count-result)]
        (swap!
          one-thread
          conj
          row)
        (when (= @count-atom
                 1)
          (swap!
            result
            conj
            @one-thread)
          (reset!
            one-thread
            (sorted-set-by
              sort-rows))
         )
        (swap!
          rows-per-thread-seq
          utils/replace-in-vector-on-index
          (atom (dec @count-atom))
          count-result))
     )
  @result))

(defn maching-unknown-signs-fn
  "Match unknown signs from passed set"
  [all-signs
   read-signs
   image-byte-array
   space-value
   matching-value]
  (let [unknown-signs-images (atom [])
        read-text (atom "")
        limit-count (atom 0)]
    (doseq [[[y-min y-max] row] all-signs]
      (let [previous-sign-x-max (atom (Integer/MAX_VALUE))]
        (doseq [[[x-min x-max] sign-dots] row]
          (try
            (let [zero-coordinates (bring-to-zero-coordinates-fn
                                     sign-dots)
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
                  read-text
                  str
                  " "))
              (if (> matching
                     matching-value)
                (swap!
                  read-text
                  str
                  sign)
                (do
                  (swap!
                    read-text
                    str
                    "*")
                  (when (< @limit-count
                           1)
                    (draw-sign image zero-coordinates)
                    (swap!
                      unknown-signs-images
                      conj
                      image)
                    (swap! limit-count inc))
                 ))
             )
            (catch IOException e
              (println "Error")
              (println (.getMessage e))
             ))
          (reset!
            previous-sign-x-max
            x-max))
       )
      (swap!
        read-text
        str
        "\n"))
    [@read-text
     @unknown-signs-images]))

(defn match-unknown-signs-tasks-fn
  "Match unknown signs from image"
  [refs
   params-n-times]
  (map
    (fn [rows-set
         [read-signs
          image-byte-array
          space-value
          matching-value]
         thread-number]
      (fn []
        (dosync
          (let [matching-result (maching-unknown-signs-fn
                                  rows-set
                                  read-signs
                                  image-byte-array
                                  space-value
                                  matching-value)]
            [thread-number
             matching-result]))
       ))
    refs
    params-n-times
    (range (count refs))
   ))

(defn match-unknown-signs-fn
  "Match unknown signs from image
   
   Multiple threads will be running when processing this function"
  [all-signs
   read-signs
   threads-value
   image-byte-array
   space-value
   matching-value]
  (let [final-result-text (atom "")
        final-result-unknown-signs-images (atom [])
        refs (divide-rows
               all-signs
               threads-value)
        new-thread-value (count refs)
        params-n-times (repeat
                         new-thread-value
                         [read-signs
                          image-byte-array
                          space-value
                          matching-value])
        tasks (match-unknown-signs-tasks-fn
                refs
                params-n-times)
        result (apply
                 conj
                 (sorted-set)
                 (apply pcalls tasks))]
    (doseq [[_ [text unknown-signs-images]] result]
      (swap!
        final-result-text
        str
        text)
      (doseq [unknown-sign-image unknown-signs-images]
        (swap!
          final-result-unknown-signs-images
          conj
          unknown-sign-image))
      )
    [@final-result-text
     @final-result-unknown-signs-images]))

(defn read-image-fn
  "Read text from image"
  [image-byte-array
   light-value
   contrast-value
   space-value
   hooks-value
   matching-value
   threads-value
   rows-threads-value
   signs]
  (let [all-signs (time
                    (read-unknown-signs-fn
                      image-byte-array
                      threads-value
                      light-value
                      contrast-value
                      space-value
                      hooks-value
                      matching-value))
        read-signs (time
                     (read-signs-fn
                       signs
                       light-value
                       contrast-value))
        final-result (time
                       (match-unknown-signs-fn
                         all-signs
                         read-signs
                         rows-threads-value
                         image-byte-array
                         space-value
                         matching-value))]
    final-result))

