(ns ocr-lib.core
  (:require [clojure.set :refer [intersection difference union select]]
            [clojure.string :as cstring]
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
  (if (and (instance?
             clojure.lang.Atom
             total-pixels-value)
           (instance?
             clojure.lang.Atom
             process-images-progress-bar-value)
           @total-pixels-value
           (number?
             @total-pixels-value)
           (> @total-pixels-value
              0)
           @process-images-progress-bar-value
           (number?
             @process-images-progress-bar-value)
           (>= @total-pixels-value
               @process-images-progress-bar-value))
    (int
      (/ (* @process-images-progress-bar-value
            100)
         @total-pixels-value))
    0))

(defn process-images-reset-progress-value-fn
  "Reset progress values"
  []
  (when (and total-pixels-value
             (instance?
               clojure.lang.Atom
               total-pixels-value))
    (reset!
      total-pixels-value
      0))
  (when (and process-images-progress-bar-value
             (instance?
               clojure.lang.Atom
               process-images-progress-bar-value))
    (reset!
      process-images-progress-bar-value
      0))
 )

(defn increase-total-pixels
  "Increase total pixels for progress value"
  [width
   height]
  (when (and width
             (number?
               width)
             height
             (number?
               height)
             (instance?
               clojure.lang.Atom
               total-pixels-value)
             (number?
               @total-pixels-value))
    (swap!
      total-pixels-value
      +
      (* width
         height))
   ))

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
  (when (and image-base64
             (string?
               image-base64)
             (not
               (cstring/blank?
                 image-base64))
         )
    (let [image-byte-array (.decode
                             base64-decoder
                             image-base64)
          image-is (ByteArrayInputStream.
                     image-byte-array)
          image (ImageIO/read
                  image-is)]
      image))
 )

(defn contrast-fn
  "Return value between 0 - 255 depending on contrast-value parameter
   if current value (color parameter) is number less then 128
   value should strive for 0 or black color
   if current value (color parameter) is number greater then 127
   value should strive for 255 or white color"
  [color
   contrast-value]
  (when (and color
             (number?
               color)
             contrast-value
             (number?
               contrast-value))
    (if (< color
           128)
      (let [color (- color
                     contrast-value)]
        (if (neg?
              color)
          0
          color))
      (let [color (+ color
                     contrast-value)]
        (if (< 255
               color)
          255
          color))
     ))
 )

(defn grayscale-contrast-fn
  "Put image in gray scale mode
   and apply light and contrast values"
  [^BufferedImage image
   & [light-value
      contrast-value]]
  (when (and image
             (instance?
               BufferedImage
               image))
    (let [height (.getHeight
                   image)
          width (.getWidth
                  image)]
      (dotimes [x width]
        (dotimes [y height]
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
                a (bit-shift-left ; transparency
                    0
                    24)
                r (contrast-fn
                    avg
                    contrast-value)
                r (bit-shift-left ; red
                    r
                    16)
                g (contrast-fn
                    avg
                    contrast-value)
                g (bit-shift-left ; green
                    g
                    8)
                b (contrast-fn
                    avg
                    contrast-value)
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
      image))
 )

(defn read-black-dots-fn
  "Read coordinates of black dots of image"
  [^BufferedImage image
   & [light-value
      contrast-value]]
  (when (and image
             (instance?
               BufferedImage
               image))
    (let [all-dots (atom #{})
          width (.getWidth
                  image)
          height (.getHeight
                   image)]
      (dotimes [x width]
        (dotimes [y height]
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
 )

(defn read-black-dots-part-fn
  "Read coordinates of black dots of image part"
  [^BufferedImage image
   start-y
   end-y
   & [light-value
      contrast-value]]
  (when (and image
             (instance?
               BufferedImage
               image)
             start-y
             (number?
               start-y)
             end-y
             (number?
               end-y))
    (let [width (.getWidth
                  image)
          height (.getHeight
                   image)
          condition-i (> (dec
                           start-y)
                         -1)
          start-y-dec (if condition-i
                        (dec
                          start-y)
                        start-y)
          condition-ii (< (inc
                            end-y)
                          height)
          end-y-inc (if condition-ii
                      (inc
                        end-y)
                      end-y)]
      (reduce
        (fn [acc-all-dots
             x]
          (reduce
            (fn [acc-all-dots-i
                 y]
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
                (if (and
                        (not
                          (or (and (= start-y-dec
                                      y)
                                   condition-i)
                              (and (= (dec
                                        end-y-inc)
                                      y)
                                   condition-ii))
                         )
                        (= (contrast-fn
                             avg
                             contrast-value)
                           0))
                  (conj
                    acc-all-dots-i
                    [x y])
                  acc-all-dots-i))
             )
            acc-all-dots
            (range
              start-y-dec
              end-y-inc))
         )
        #{}
        (range
          width))
     ))
 )

(defn dot-is-black?
  "Check if dot with particular coordinates is black"
  [^BufferedImage image
   x
   y]
  (when (and image
             (instance?
               BufferedImage
               image)
             x
             (number?
               x)
             y
             (number?
               y)
             (not (neg? x))
             (not (neg? y))
             (< x (.getWidth
                    image))
             (< y (.getHeight
                    image))
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
  (if (and x
           (number?
             x)
           y
           (number?
             y))
    #{[(dec x) y]
      [(inc x) y]
      [x (dec y)]
      [x (inc y)]
      [(dec x) (inc y)]
      [(inc x) (inc y)]
      [(inc x) (dec y)]
      [(dec x) (dec y)]}
    #{}))

(defn find-sign-dots-fn
  "Find coordinates that correspond to dots of a single sign"
  [all-dots
   sign-dots
   checked-dots
   width
   height]
  (if (empty?
        (difference
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
                             (if (and width
                                      (number?
                                        width)
                                      (pos?
                                        width)
                                      (< x width)
                                      (> x -1)
                                      height
                                      (number?
                                        height)
                                      (pos?
                                        height)
                                      (< y height)
                                      (> y -1)
                                      (contains?
                                        all-dots
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
  (when (and dots-set
             (set?
               dots-set)
             (not
               (empty?
                 dots-set))
         )
    (reduce
      (fn [[acc-x-min
            acc-y-min
            acc-x-max
            acc-y-max]
           element]
        (if (vector?
              element)
          (let [[x y] element]
            [(if (< x
                    acc-x-min)
               x
               acc-x-min)
             (if (< y
                    acc-y-min)
               y
               acc-y-min)
             (if (< acc-x-max
                    x)
               x
               acc-x-max)
             (if (< acc-y-max
                    y)
               y
               acc-y-max)])
          [acc-x-min
           acc-y-min
           acc-x-max
           acc-y-max]))
      [Integer/MAX_VALUE
       Integer/MAX_VALUE
       0
       0]
      dots-set))
 )

(defn find-dots-hooks-fn
  "Find dots, hooks, dashes...
   above or below the sign"
  [all-dots
   x-min
   x-max
   y-min
   y-max
   hooks-value]
  (when (and all-dots
             (set?
               all-dots)
             x-min
             (number?
               x-min)
             x-max
             (number?
               x-max)
             y-min
             (number?
               y-min)
             y-max
             (number?
               y-max)
             hooks-value
             (number?
               hooks-value))
    (let [examining-area (reduce
                           (fn [acc-set
                                x]
                             (let [hooks-above-sign-set (reduce
                                                          (fn [acc-set-i
                                                               y]
                                                            (conj
                                                              acc-set-i
                                                              [x y]))
                                                          acc-set
                                                          (range
                                                            (- y-min
                                                               hooks-value)
                                                            (dec
                                                              y-min))
                                                         )]
                               (reduce
                                 (fn [acc-set-i
                                      y]
                                   (conj
                                     acc-set-i
                                     [x y]))
                                 hooks-above-sign-set
                                 (range
                                   (inc
                                     y-max)
                                   (+ y-max
                                      hooks-value))
                                ))
                            )
                           #{}
                           (range
                             x-min
                             x-max))]
      (intersection
        all-dots
        examining-area))
   ))

(defn sort-row-elements
  "Function that sorts set elements
   (set is representing row with signs)"
  [[[x-min1 x-max1] elem1]
   [[x-min2 x-max2] elem2]]
  (when (and (number?
               x-min1)
             (number?
               x-min2)
             (number?
               x-max1)
             (number?
               x-max2))
    (or (< x-min1 x-min2)
        (< x-max1 x-max2))
   ))

(defn sort-rows
  "Function that sorts set elements
   (set is representing page with rows)"
  [[[y-min1 y-max1] elem1]
   [[y-min2 y-max2] elem2]]
  (when (and (number?
               y-min1)
             (number?
               y-max1)
             (number?
               y-min2)
             (number?
               y-max2))
    (and (< y-max1 y-min2)
         (< y-min1 y-max2))
   ))

(defn add-into-sorted-set-fn
  "Add sign into sorted set"
  [dots-sets-of-signs
   x-min
   y-min
   x-max
   y-max
   sign-dots]
  (when (and dots-sets-of-signs
             (set?
               dots-sets-of-signs)
             x-min
             (number?
               x-min)
             y-min
             (number?
               y-min)
             x-max
             (number?
               x-max)
             y-max
             (number?
               y-max)
             sign-dots
             (set?
               sign-dots))
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
      (if (empty?
            row)
        (conj
          dots-sets-of-signs
          [[y-min y-max]
           (conj
             row-element
             [[x-min x-max] sign-dots])])
        (let [dots-sets-of-signs (difference
                                   dots-sets-of-signs
                                   row)
              first-row (first
                          row)
              [[y-min-r y-max-r] elem-r] first-row
              [new-row
               y-min-d
               y-max-d]
                (reduce
                  (fn [[acc-new-row
                        acc-y-min-d
                        acc-y-max-d]
                       [[y-min-s y-max-s] elem-s]]
                    [(apply
                       conj
                       acc-new-row
                       elem-s)
                     (if (< y-min-s
                            acc-y-min-d)
                       y-min-s
                       acc-y-min-d)
                     (if (< acc-y-max-d
                            y-min-s)
                       y-max-s
                       acc-y-max-d)])
                  [(sorted-set-by
                     sort-row-elements)
                   (if (< y-min
                          y-min-r)
                     y-min
                     y-min-r)
                   (if (< y-max
                          y-max-r)
                     y-max-r
                     y-max)]
                  row)
              new-row (conj
                        new-row
                        [[x-min x-max] sign-dots])]
          (conj
            dots-sets-of-signs
            [[y-min-d y-max-d] new-row]))
       ))
   ))

(defn grouping-dots-fn
  "Grouping dots into sets which will represent separated signs"
  [all-dots
   dots-sets-of-signs
   width
   height
   hooks-value]
  (when (and all-dots
             (set?
               all-dots)
             dots-sets-of-signs
             (set?
               dots-sets-of-signs)
             width
             (number?
               width)
             height
             (number?
               height)
             hooks-value
             (number?
               hooks-value))
    (if (empty?
          all-dots)
      dots-sets-of-signs
      (let [[x y] (first
                    all-dots)
            all-dots (disj
                       all-dots
                       [x y])
            [all-dots
             sign-dots] (find-sign-dots-fn
                          all-dots
                          #{[x y]}
                          #{}
                          width
                          height)
            [x-min y-min
             x-max y-max] (find-min-max-fn
                            sign-dots)
            dots-hooks-set (find-dots-hooks-fn
                             all-dots
                             x-min
                             x-max
                             y-min
                             y-max
                             hooks-value)
            [all-dots
             sign-dots] (if (empty?
                              dots-hooks-set)
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
            [x-min y-min
             x-max y-max] (find-min-max-fn
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
 )

(defn read-signs-fn
  "Read known signs images from byte arrays"
  [signs
   light-value
   contrast-value]
  (when (and signs
             (map?
               signs))
    (let [signs-keys (keys
                       signs)]
      (reduce
        (fn [acc-map
             s-key]
          (let [images-sign-vector (s-key
                                     signs)]
            (assoc
              acc-map
              s-key
              (if (vector?
                    images-sign-vector)
                (reduce
                  (fn [acc-vec
                       image-byte-array]
                    (let [image (ImageIO/read
                                  (ByteArrayInputStream.
                                    image-byte-array))
                          all-dots (read-black-dots-fn
                                     image
                                     light-value
                                     contrast-value)]
                     (conj
                       acc-vec
                       all-dots))
                   )
                  []
                  images-sign-vector)
                []))
           ))
        {}
        signs-keys))
   ))

(defn bring-to-zero-coordinates-fn
  "Bring coordinates from dots-set parameter to minimal positive values"
  [dots-set]
  (let [[x-min y-min
         x-max y-max] (find-min-max-fn
                        dots-set)]
    (reduce
      (fn [acc-set
           element]
        (if (vector?
              element)
          (let [[x y] element]
            (conj
              acc-set
              [(- x x-min) (- y y-min)])
           )
          acc-set))
      #{}
      dots-set))
 )

(defn check-matching-fn
  "Check matching of zero coordinates unknown sign
   with all known signs and return sign and matching percentage"
  [zero-coordinates
   read-signs]
  (let [signs-keys (keys
                     read-signs)]
    (reduce
      (fn [{acc-sign :sign
            acc-matching :matching}
           s-key]
        (let [known-sign-vector (s-key
                                  read-signs)]
          (if (vector?
                known-sign-vector)
            (reduce
              (fn [{acc-sign-i :sign
                    acc-matching-i :matching}
                   dots-set]
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
                                               union-count))]
                  (if (< acc-matching-i
                         matching-percentage)
                    {:sign (name
                             s-key)
                     :matching matching-percentage}
                    {:sign acc-sign-i
                     :matching acc-matching-i}))
               )
              {:sign acc-sign
               :matching acc-matching}
              (s-key
                read-signs))
            {:sign acc-sign
             :matching acc-matching}))
       )
      {:sign ""
       :matching 0}
      signs-keys))
 )

(defn draw-sign
  "Draw image in image parameter"
  [^BufferedImage image
   coordinates-set]
  (when (and image
             (instance?
               BufferedImage
               image))
    (let [width (.getWidth
                  image)
          height (.getHeight
                   image)]
      (dotimes [x width]
        (dotimes [y height]
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
     ))
  image)

(defn grouping-dots-one-part-fn
  "Grouping dots from one part of image"
  [^BufferedImage image
   thread-number
   n-threads
   light-value
   contrast-value
   hooks-value]
  (when (and (instance?
               BufferedImage
               image)
             (number?
               n-threads)
             (number?
               thread-number)
             (number?
               hooks-value))
    (let [width (.getWidth
                  image)
          height (.getHeight
                   image)
          one-part-height (if (and n-threads
                                   (number?
                                     n-threads)
                                   (not= n-threads
                                         0))
                            (int
                              (/ height
                                 n-threads))
                            height)
          last-part-height (if (= n-threads
                                  (inc
                                    thread-number))
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
 )

(defn merge-and-disj
  "Merge sign parts from selected set and remove them from row that contains them"
  [selected-signs-set
   new-x-min
   new-x-max
   new-sign
   row]
  (when (and (set?
               selected-signs-set)
             (number?
               new-x-min)
             (number?
               new-x-max)
             (set?
               new-sign))
    (reduce
      (fn [[acc-new-x-min
            acc-new-x-max
            acc-new-sign
            acc-row]
           [[x-min x-max] sign]]
        [(if (< x-min
                acc-new-x-min)
           x-min
           acc-new-x-min)
         (if (< acc-new-x-max
                x-max)
           x-max
           acc-new-x-max)
         (union
           acc-new-sign
           sign)
         (disj
           acc-row
           [[x-min x-max] sign])])
      [new-x-min
       new-x-max
       new-sign
       row]
      selected-signs-set))
 )

(defn merge-sign-coordinates-concrete
  "Merge sign parts from both row parts"
  [row-f
   row-s
   new-sign
   new-x-min
   new-x-max]
  (when (and (set?
               row-f)
             (set?
               row-s)
             (set?
               new-sign)
             (number?
               new-x-min)
             (number?
               new-x-max))
    (let [select-pred? (fn [[[x-min x-max] sign]]
                         (when (and (number?
                                      x-min)
                                    (number?
                                      x-max))
                           (or (<= x-min new-x-min x-max)
                               (<= x-min new-x-max x-max)
                               (<= new-x-min x-min new-x-max)
                               (<= new-x-min x-max new-x-max))
                          ))
          selected-signs-set-f (select
                                 select-pred?
                                 row-f)
          selected-signs-set-s (select
                                 select-pred?
                                 row-s)]
      (if (and (empty?
                 selected-signs-set-f)
               (empty?
                 selected-signs-set-s))
        [[[new-x-min
           new-x-max]
          new-sign]
         row-f
         row-s]
        (let [[new-x-min
               new-x-max
               new-sign
               row-f] (merge-and-disj
                        selected-signs-set-f
                        new-x-min
                        new-x-max
                        new-sign
                        row-f)
              [new-x-min
               new-x-max
               new-sign
               row-s] (merge-and-disj
                        selected-signs-set-s
                        new-x-min
                        new-x-max
                        new-sign
                        row-s)]
          (recur
            row-f
            row-s
            new-sign
            new-x-min
            new-x-max))
       ))
   ))

(defn merge-sign-coordinates
  "Merge sign parts, sign by sign, and return merged row"
  [row-f
   row-s
   result
   new-sign
   new-x-min
   new-x-max]
  (when (and (set?
               row-f)
             (set?
               row-s)
             (set?
               result)
             (set?
               new-sign)
             (number?
               new-x-min)
             (number?
               new-x-max))
    (let [condition-i (empty?
                        row-f)
          condition-ii (empty?
                         row-s)
          condition-iii (and (not condition-i)
                             condition-ii)
          condition-iv (and (not condition-ii)
                            condition-i)
          condition-v (and condition-i
                           condition-ii)
          condition-vi (and (not condition-i)
                            (not condition-ii))
          condition-vii (empty?
                          new-sign)]
      (if condition-v
        result
        (if condition-iii
          (recur
            (empty
              row-f)
            row-s
            (apply
              conj
              result
              row-f)
            new-sign
            new-x-min
            new-x-max)
          (if condition-iv
            (recur
              row-f
              (empty
                row-s)
              (apply
                conj
                result
                row-s)
              new-sign
              new-x-min
              new-x-max)
            (let [[[x-min-f x-max-f] sign-f] (first
                                               row-f)
                  row-f (disj
                          row-f
                          [[x-min-f x-max-f] sign-f])
                  [new-sign-result
                   row-f
                   row-s] (merge-sign-coordinates-concrete
                            row-f
                            row-s
                            sign-f
                            x-min-f
                            x-max-f)
                  result (conj
                           result
                           new-sign-result)]
               (recur
                 row-f
                 row-s
                 result
                 #{}
                 Integer/MAX_VALUE
                 0))
              ))
       ))
   ))

(defn merge-separated-parts
  "Merge parts separated by partial image processing"
  [sorted-rows-set]
  (when (and sorted-rows-set
             (set?
               sorted-rows-set))
    (let [{[[y-min-p y-max-p] row-p] :previous
           result :result} (reduce
                             (fn [{acc-result :result
                                   acc-previous :previous}
                                  [[y-min-c
                                    y-max-c]
                                   row-c]]
                               (if-let [[[y-min-p y-max-p] row-p] acc-previous]
                                 (if (= y-min-c
                                        (inc
                                          y-max-p))
                                   {:result (conj
                                              acc-result
                                              [[y-min-p
                                                y-max-c]
                                               (merge-sign-coordinates
                                                 row-p
                                                 row-c
                                                 (sorted-set-by
                                                   sort-row-elements)
                                                 #{}
                                                 Integer/MAX_VALUE
                                                 0)])
                                    :previous [[y-min-c
                                                y-max-c]
                                               row-c]}
                                   {:result (conj
                                              acc-result
                                              [[y-min-p y-max-p] row-p])
                                    :previous [[y-min-c
                                                y-max-c]
                                               row-c]})
                                 {:result acc-result
                                  :previous [[y-min-c
                                              y-max-c]
                                             row-c]}))
                             {:result (sorted-set-by
                                        sort-rows)
                              :previous nil}
                             sorted-rows-set)
          [[y-min-l y-max-l] row-l] (last
                                      result)
          y-min-l (or y-min-l
                      -1)
          y-max-l (or y-max-l
                      -1)
          y-min-p (or y-min-p
                      -1)
          y-max-p (or y-max-p
                      -1)
          previous-merged (or (<= y-min-p y-min-l y-max-p)
                              (<= y-min-p y-max-l y-max-p)
                              (<= y-min-l y-min-p y-max-l)
                              (<= y-min-l y-max-p y-max-l))
          final-result (if previous-merged
                         result
                         (conj
                           result
                           [[y-min-p y-max-p] row-p]))]
      final-result))
 )

(defn read-unknown-signs-tasks-fn
  "Read unknown signs from image, n-threads parts in parallel"
  [image
   light-value
   contrast-value
   space-value
   hooks-value
   n-threads]
  (when n-threads
    (map
      (fn [thread-number]
        #(let [all-signs (grouping-dots-one-part-fn
                           image
                           thread-number
                           n-threads
                           light-value
                           contrast-value
                           hooks-value)]
           all-signs))
      (range
        n-threads))
   ))

(defn read-unknown-signs-fn
  "Read unknown signs from image
   
   Multiple threads will be running when processing this function"
  [image-byte-array
   n-threads
   light-value
   contrast-value
   space-value
   hooks-value]
  (when (and image-byte-array
             (bytes?
               image-byte-array))
    (let [n-threads (or n-threads
                        1)
          image-obj (ImageIO/read
                      (ByteArrayInputStream.
                        image-byte-array))
          tasks (read-unknown-signs-tasks-fn
                  image-obj
                  light-value
                  contrast-value
                  space-value
                  hooks-value
                  n-threads)
          result (apply
                   union
                   (sorted-set-by
                     sort-rows)
                   (apply
                     pcalls
                     tasks))]
      (merge-separated-parts
        result))
   ))

(defn divide-rows
 "Divide rows into separate sets for multi processing"
 [all-signs
  threads]
 (when (and threads
            (number?
              threads)
            (pos?
              threads))
   (let [rows (count
                all-signs)
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
     (dotimes [i mod-threads]
       (let [rows-per-thread-doseq (get
                                     @rows-per-thread-seq
                                     i)]
         (swap!
           rows-per-thread-seq
           utils/replace-in-vector-on-index
           (atom
             (inc
               @rows-per-thread-doseq))
           i))
      )
     (doseq [row all-signs]
       (let [count-result (count
                            @result)
             count-atom (get
                          @rows-per-thread-seq
                          count-result)]
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
           (atom
             (dec
               @count-atom))
           count-result))
      )
     @result))
 )

(defn maching-unknown-signs-fn
  "Match unknown signs from passed set"
  [all-signs
   read-signs
   image-byte-array
   space-value
   matching-value
   unknown-sign-count-limit-per-thread]
  (reduce
    (fn [[acc-read-text
          acc-unknown-signs-images
          acc-limit-count]
         [[y-min y-max] row]]
      (let [[res-read-text
             res-unknown-signs-images
             res-limit-count]
             (reduce
               (fn [[acc-read-text-i
                     acc-unknown-signs-images-i
                     acc-limit-count-i
                     acc-previous-sign-x-max-i]
                    [[x-min x-max] sign-dots]]
                 (try
                   (let [zero-coordinates (bring-to-zero-coordinates-fn
                                            sign-dots)
                         {sign :sign
                          matching :matching} (check-matching-fn
                                                zero-coordinates
                                                read-signs)
                         space-area (- x-min
                                       acc-previous-sign-x-max-i)]
                     [(str
                        acc-read-text-i
                        (when (> space-area
                                 (or space-value
                                     16))
                          " ")
                        (if (> matching
                               (or matching-value
                                   70))
                          sign
                          "*"))
                      (if (> matching
                             (or matching-value
                                 70))
                        acc-unknown-signs-images-i
                        (if (< acc-limit-count-i
                               (or unknown-sign-count-limit-per-thread
                                   1))
                          (conj
                            acc-unknown-signs-images-i
                            (let [[_ _ x-max y-max] (find-min-max-fn
                                                      zero-coordinates)
                                  width (inc
                                          x-max)
                                  height (inc
                                           y-max)
                                  image (.getSubimage
                                          (ImageIO/read
                                            (ByteArrayInputStream.
                                              image-byte-array))
                                          0
                                          0
                                          width
                                          height)]
                              (draw-sign
                                image
                                zero-coordinates)
                              image))
                          acc-unknown-signs-images-i))
                      (if (and (not
                                 (> matching
                                    (or matching-value
                                        70))
                                )
                               (< acc-limit-count-i
                                  (or unknown-sign-count-limit-per-thread
                                      1))
                           )
                        (inc
                          acc-limit-count-i)
                        acc-limit-count-i)
                      x-max])
                   (catch IOException e
                     (println "Error")
                     (println (.getMessage e))
                     [acc-read-text-i
                      acc-unknown-signs-images-i
                      acc-limit-count-i
                      acc-previous-sign-x-max-i]))
                )
               [acc-read-text
                acc-unknown-signs-images
                acc-limit-count
                Integer/MAX_VALUE]
               row)]
        [(str
           res-read-text
           "\n")
         res-unknown-signs-images
         res-limit-count]))
    [""
     []
     0]
    all-signs))

(defn match-unknown-signs-tasks-fn
  "Match unknown signs from image"
  [refs
   read-signs
   image-byte-array
   space-value
   matching-value
   unknown-sign-count-limit-per-thread]
  (map
    (fn [rows-set
         thread-number]
      #(let [matching-result (maching-unknown-signs-fn
                               rows-set
                               read-signs
                               image-byte-array
                               space-value
                               matching-value
                               unknown-sign-count-limit-per-thread)]
         [thread-number
          matching-result]))
    refs
    (range
      (count
        refs))
   ))

(defn match-unknown-signs-fn
  "Match unknown signs from image
   
   Multiple threads will be running when processing this function"
  [all-signs
   read-signs
   threads-value
   image-byte-array
   space-value
   matching-value
   unknown-sign-count-limit-per-thread]
  (let [threads-value (or threads-value
                          1)
        refs (divide-rows
               all-signs
               threads-value)
        tasks (match-unknown-signs-tasks-fn
                refs
                read-signs
                image-byte-array
                space-value
                matching-value
                unknown-sign-count-limit-per-thread)
        result (apply
                 conj
                 (sorted-set)
                 (apply
                   pcalls
                   tasks))
        {final-result-text :text
         final-result-unknown-signs-images :unknown-signs-images}
          (reduce
            (fn [{acc-text :text
                  acc-unknown-signs-images :unknown-signs-images}
                 [_ [text
                     unknown-signs-images]]]
              {:text (str
                       acc-text
                       text)
               :unknown-signs-images (apply
                                       conj
                                       acc-unknown-signs-images
                                       unknown-signs-images)})
            {:text ""
             :unknown-signs-images []}
            result)]
    [final-result-text
     final-result-unknown-signs-images]))

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
   signs
   unknown-sign-count-limit-per-thread]
  (let [all-signs (time
                    (read-unknown-signs-fn
                      image-byte-array
                      threads-value
                      light-value
                      contrast-value
                      space-value
                      hooks-value))
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
                         matching-value
                         unknown-sign-count-limit-per-thread))]
    final-result))

