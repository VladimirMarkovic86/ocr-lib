(ns ocr.core
 (:require [clojure.java.io :refer [resource file writer]]
           [clojure.set :refer [intersection difference union]])
 (:import [java.io File
                   IOException]
          [java.awt.image BufferedImage]
          [javax.imageio ImageIO]
          [java.util.concurrent Executors]))

(def slova (atom []))

(def min-width (atom (Integer/MAX_VALUE)))

(def max-width (atom 0))

(def min-height (atom (Integer/MAX_VALUE)))

(def max-height (atom 0))

(def min-dots (atom (Integer/MAX_VALUE)))

(def max-dots (atom 0))

(def razmak 35)

(def min-tacke-kukice 35)

(defn clone-image
 ""
 [image]
 (let [cm (.getColorModel image)
       isAlphaPremultiplied (.isAlphaPremultiplied cm)
       raster (.copyData image nil)
       copied-image (BufferedImage. cm raster isAlphaPremultiplied nil)]
  copied-image))

(defn contrast
 ""
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

(defn change-pixel
 ""
 [image
  x
  y]
 (let [a (bit-shift-left 127 24) ; transparency
       r (bit-shift-left 0 16) ; red
       g (bit-shift-left 0 8) ; green
       b (bit-shift-left 0 0) ; blue
       p (bit-or a r g b)]
  (.setRGB image x y p)
  )
 )

(defn write-line
 ""
 [image
  x
  y]
 (let [x-range (range x (+ x 7))
       x-max (+ x 6)
       y-range (range y (+ y 11))
       y-max (+ y 11)]
  (doseq [y y-range]
   (change-pixel image x y)
   )
  (doseq [x x-range]
   (change-pixel image x y)
   )
  (doseq [y y-range]
   (change-pixel image x-max y)
   )
  (doseq [x x-range]
   (change-pixel image x y-max)
   )
  )
 )

(defn iterate-x-y
 ""
 [image
  x
  y]
 (doseq [x (range 1 3089 9)]
  (doseq [y (range 1 4115 13)]
   (write-line image x y)
   )
  )
 )

(defn proveri-slovo
 ""
 [image
  x
  y
  parovi
  boja]
 (let [poklapanja (atom 0)]
  (doseq [[x-par y-par] parovi]
   (let [p (.getRGB image (+ x x-par)
                          (+ y y-par))
         r (bit-and (bit-shift-right p 16) ; red
                    255)
         g (bit-and (bit-shift-right p 8) ; green
                    255)
         b (bit-and (bit-shift-right p 0) ; blue
                    255)]
;   (println (str " x: " (+ x x-par) " y: " (+ y y-par)))
    (if (and (= r boja)
             (= g boja)
             (= b boja))
     (swap! poklapanja inc)
     nil))
   )
  @poklapanja))

(defn find-min-max
 ""
 [skup-tacaka]
 (let [x-min (atom (Integer/MAX_VALUE))
       y-min (atom (Integer/MAX_VALUE))
       x-max (atom 0)
       y-max (atom 0)]
  (doseq [[x y] skup-tacaka]
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

(defn svedi-na-nulte-koordinate
 ""
 [skup-tacaka]
 (let [[x-min y-min x-max y-max] (find-min-max skup-tacaka)
       novi-skup-tacaka (atom #{})]
  (doseq [[x y] skup-tacaka]
   (swap! novi-skup-tacaka conj [(- x x-min) (- y y-min)]))
  @novi-skup-tacaka))

(defn procitaj-nepoznato-slovo
 ""
 [image
  start
  end
  height]
 (let [black-pixels (atom #{})]
  (doseq [y (range height)]
   (doseq [x (range start end)]
    (let [p (.getRGB image x y)
          a (bit-and (bit-shift-right p 24) ; transparency
                     127)
          r (bit-and (bit-shift-right p 16) ; red
                     255)
          g (bit-and (bit-shift-right p 8) ; green
                     255)
          b (bit-and (bit-shift-right p 0) ; blue
                     255)
          avg (int (/ (+ r g b) 3))
          r (contrast avg 128)
          g (contrast avg 128)
          b (contrast avg 128)]
     (when (and (= r 0)
                (= g 0)
                (= b 0))
      (swap! black-pixels conj [x y]))
     ))
   )
; (println @black-pixels)
 (svedi-na-nulte-koordinate @black-pixels))
 )

(defn grayscale-contrast
 ""
 [image
  width
  height
  & [light-value]]
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
         r (contrast avg 128)
         g (contrast avg 128)
         b (contrast avg 128)
         a (bit-shift-left 0 24) ; transparency
         r (bit-shift-left r 16) ; red
         g (bit-shift-left g 8) ; green
         b (bit-shift-left b 0) ; blue
         p (bit-or a r g b)
         ]
    (.setRGB image x y p)
    ))
  ))

(defn find-max
 [acc
  {slovo :slovo
   broj-poklopljenih-tacaka :broj-poklopljenih-tacaka
   razlika :razlika}]
  (if (or (< (:broj-poklopljenih-tacaka acc)
             broj-poklopljenih-tacaka)
          (and (= (:broj-poklopljenih-tacaka acc)
                  broj-poklopljenih-tacaka)
               (< razlika
                  (:razlika acc))))
   {:slovo slovo
    :broj-poklopljenih-tacaka broj-poklopljenih-tacaka
    :razlika razlika}
   acc))

(defn most-probable-vektor
 [results]
 (reduce find-max {:slovo nil
                   :broj-poklopljenih-tacaka 0
                   :razlika (Integer/MAX_VALUE)} results))

(defn pomeri-skup-tacaka
 ""
 [skup-tacaka
  x-pomeraj
  y-pomeraj]
 (let [novi-skup-tacaka (atom #{})]
  (doseq [[x y] skup-tacaka]
   (swap! novi-skup-tacaka conj [(+ x x-pomeraj) (+ y y-pomeraj)])
   )
  @novi-skup-tacaka))

(defn pronadji-slovo
 ""
 [image
  start
  end
  height
  tekst
  prethodni-end]
 (let [vektor-nepoznatog-slova (procitaj-nepoznato-slovo image start end height)
       vektor-nepoznatog-slova-na-gore (pomeri-skup-tacaka
                                        vektor-nepoznatog-slova
                                        0
                                        -1)
       vektor-nepoznatog-slova-u-levo (pomeri-skup-tacaka
                                       vektor-nepoznatog-slova
                                       -1
                                       0)
       vektor-nepoznatog-slova-u-levo-i-na-gore (pomeri-skup-tacaka
                                                 vektor-nepoznatog-slova
                                                 -1
                                                 -1)
;       test-a (println vektor-nepoznatog-slova)
       poklapanja-slova (atom [])]
  (doseq [[tacke slovo sirina visina] @slova]
   (let [p0 (count (intersection tacke
                                 vektor-nepoznatog-slova))
         r0 (count (difference tacke
                               vektor-nepoznatog-slova))
         p1 (count (intersection tacke
                                 vektor-nepoznatog-slova-na-gore))
         r1 (count (difference tacke
                               vektor-nepoznatog-slova-na-gore))
         p2 (count (intersection tacke
                                 vektor-nepoznatog-slova-u-levo))
         r2 (count (difference tacke
                               vektor-nepoznatog-slova-u-levo))
         p3 (count (intersection tacke
                                 vektor-nepoznatog-slova-u-levo-i-na-gore))
         r3 (count (difference tacke
                               vektor-nepoznatog-slova-u-levo-i-na-gore))
         poklopljene-tacke (int (/ (+ p0 p1 p2 p3) 4))
         ne-poklopljene-tacke (int (/ (+ r0 r1 r2 r3) 4))
         ;broj-tacaka-slova (count tacke)
         ;broj-tacaka-nepoznatog-slova (count vektor-nepoznatog-slova)
         ]
    ;(println (char slovo) " " p0 " " r0 " " p1 " " r1 " " p2 " " r2 " " p3 " " r3 " " broj-tacaka-slova " " broj-tacaka-nepoznatog-slova)
    ;(println (char slovo) " " poklopljene-tacke " " ne-poklopljene-tacke " " broj-tacaka-slova " " broj-tacaka-nepoznatog-slova)
    (swap! poklapanja-slova conj
     {:slovo (char slovo)
      :broj-poklopljenih-tacaka poklopljene-tacke
      :razlika ne-poklopljene-tacke}))
   )
  ;(println @poklapanja-slova)
  ;(println (most-probable-vektor @poklapanja-slova) (- end start))
  (when (< 16
           (- start
              @prethodni-end))
   (swap! tekst str " "))
  (reset! prethodni-end end)
  (:slovo (most-probable-vektor @poklapanja-slova))
  ;(alter refs str (:slovo (most-probable-vektor @poklapanja-slova))
  ; )
  )
 )

(defn citaj
 ""
 [image
  height
  ob-slova
  thread-number]
 (let [tekst (atom "")
       prethodni-end (atom 0)]
  (doseq [[start end] ob-slova]
   (when (< 8
            (- end
               start))
    (if (< (- end
              start)
           37)
     (swap! tekst str (pronadji-slovo image start end height tekst prethodni-end))
     (let [start-a (atom start)
           end-a (atom (+ start
                          35))]
      (while (< 10 (- end @start-a))
       ;(println @start-a " " @end-a " " end)
       (let [slovo (pronadji-slovo image @start-a @end-a height tekst prethodni-end)]
        (swap! tekst str slovo)
        (doseq [[tacke slovo-a sirina visina] @slova]
         (when (= (char slovo-a)
                  slovo)
          (swap! start-a + sirina)
          (swap! end-a + 35)
          ))
        )
       )
      )
     )
    )
   )
  [thread-number @tekst]
  )
 )

(defn procitaj-slovo
 ""
 [image
  width
  height
  slovo]
 (let [black-pixels (atom #{})]
  (doseq [y (range height)]
   (doseq [x (range width)]
    (let [p (.getRGB image x y)
          a (bit-and (bit-shift-right p 24) ; transparency
                     127)
          r (bit-and (bit-shift-right p 16) ; red
                     255)
          g (bit-and (bit-shift-right p 8) ; green
                     255)
          b (bit-and (bit-shift-right p 0) ; blue
                     255)
          avg (int (/ (+ r g b) 3))
          r (contrast avg 128)
          g (contrast avg 128)
          b (contrast avg 128)]
     (if (and (= r 0)
              (= g 0)
              (= b 0))
      (swap! black-pixels conj [x y])
      nil))
    ))
  (swap! slova conj [@black-pixels (int slovo) width height])
  )
 )

(defn oblast-slova
 ""
 [x-axis-vektor]
 (let [oblasti-slova (atom [])
       oblast-slova (atom [])]
  (doseq [x x-axis-vektor]
   (when-let [[start end] @oblast-slova]
    (if (= start
           nil)
     (reset! oblast-slova [x])
     (if (= (inc start)
            x)
      (reset! oblast-slova [x])
      (do
       (swap! oblasti-slova conj [start x])
       (reset! oblast-slova [x]))
      ))
    ))
  @oblasti-slova))

(defn odredi-oblast-slova
 ""
 [image
  width
  height]
 (let [x (atom 0)
       white-columns (atom [])]
  (while (< @x width)
   (let [y (atom 0)]
    (while (< @y height)
     (let [p (.getRGB image @x @y)
           a (bit-and (bit-shift-right p 24) ; transparency
                      127)
           r (bit-and (bit-shift-right p 16) ; red
                      255)
           g (bit-and (bit-shift-right p 8) ; green
                      255)
           b (bit-and (bit-shift-right p 0) ; blue
                      255)]
      (if (and (= r 255)
               (= g 255)
               (= b 255))
       (swap! y inc)
       (swap! y + 1 height))
      ))
     (when-not (< height @y)
      (swap! white-columns conj @x))
    )
    (swap! x inc))
  (oblast-slova @white-columns)))

(defn tacka-je-crna?
 ""
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

(defn koordinate-okoline-skup
 ""
 [x y]
 #{[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]
   [(dec x) (inc y)]
   [(inc x) (inc y)]
   [(inc x) (dec y)]
   [(dec x) (dec y)]})

(defn proveri-okolinu
 ""
 [image
  skup-tacaka
  koord]
 ;(println " skup-tacaka: " skup-tacaka)
 ;(println " koord: " koord)
 (if (and (< (count skup-tacaka)
             20)
          (empty? koord))
  skup-tacaka
  (if (< (count skup-tacaka)
         20)
   (let [skup-tacaka (atom skup-tacaka)
         koord (atom koord)]
    (doseq [[x y] @koord]
     (swap! koord disj [x y])
     (when (tacka-je-crna? image x y)
      (swap! skup-tacaka conj [x y])
      (swap!
       koord
       (fn [k nt]
        (reduce
         (fn [acc element]
          (if (contains? @skup-tacaka element)
           acc
           (conj acc element))
          )
         k
         nt))
       (koordinate-okoline-skup x y))
      ))
    (recur image @skup-tacaka @koord))
   nil))
 )

(defn obrisi-piksele
 ""
 [image
  width
  height]
 (doseq [x (range width)]
  (doseq [y (range height)]
   (try
    (let [p (.getRGB image x y)
          a (bit-and (bit-shift-right p 24) ; transparency
                     127)
          r (bit-and (bit-shift-right p 16) ; red
                     255)
          g (bit-and (bit-shift-right p 8) ; green
                     255)
          b (bit-and (bit-shift-right p 0) ; blue
                     255)]
     (when (and (= r
                   0)
                (= g
                   0)
                (= b
                   0))
      (when-let [skup-tacaka (proveri-okolinu image #{[x y]} (koordinate-okoline-skup x y))]
       (doseq [[x y] skup-tacaka]
        ;(println [x y])
        (let [r 255
              g 255
              b 255
              a (bit-shift-left 0 24) ; transparency
              r (bit-shift-left r 16) ; red
              g (bit-shift-left g 8) ; green
              b (bit-shift-left b 0) ; blue
              p (bit-or a r g b)]
         (.setRGB image x y p))
        ))
      ))
    (catch Exception e
     ;(println (.getMessage e))
     ))
   ))
 ;(println "kraj")
 )

(defn rw-image
  ""
  [url
   width
   height
   izvrsi-fn
   & [opciono]]
  (try
   (let [fin (resource url)
         ;fout (file "resources/out1.jpg")
         ]
    (if (nil? fin)
     (println "file not found or there was a problem reading it")
     (let [image (BufferedImage. width
                                 height
                                 (BufferedImage/TYPE_INT_ARGB))
           image (ImageIO/read fin)
           width (.getWidth image)
           height (.getHeight image)]
      (when (= "procitaj-slovo"
               izvrsi-fn)
       (procitaj-slovo image width height opciono))
      (when (= "procitaj-tekst"
               izvrsi-fn)
       (grayscale-contrast image width height)
       (obrisi-piksele image width height)
       (let [ob-slova (odredi-oblast-slova image width height)]
        (citaj image height ob-slova opciono))
       )
;      (println width)
;      (println height)
;      
;      (iterate-x-y image 1 1)
;      (ImageIO/write image "jpg" fout))
     )
    ))
   (catch IOException e
    (println "greska")
    (println (.getMessage e))
    ))
  )

(defn tasks-fn
 [vektor-slika
  refs]
 (map (fn [[url width height] thread-number]
       (fn []
        (dosync
         (swap! refs conj (rw-image url width height "procitaj-tekst" thread-number))
         )
        )
      )
     vektor-slika
     (range (count vektor-slika))
  ))

(defn multi-tasking
 [vektor-slika]
 (let [refs (atom (sorted-set))
       pool (Executors/newFixedThreadPool 5)
       tasks (tasks-fn vektor-slika refs)]
  (doseq [future (.invokeAll pool tasks)]
   (.get future)
   )
  (.shutdown pool)
;  (println tasks)
;  (println @refs)
  (doseq [[thread-number red] @refs]
   (println red)
   )
;  (deref refs)
;  (doseq [red (deref refs)]
;   (println red)
;   )
  ))

(defn tasks-fn
 [vektor-slika
  refs]
 (map (fn [[image width height] thread-number]
       (fn []
        (dosync
         (swap! refs conj (let [ob-slova (odredi-oblast-slova image width height)]
                           (citaj image height ob-slova thread-number))
          )
         )
        )
      )
     vektor-slika
     (range (count vektor-slika))
  ))

(defn multi-tasking-slika
 [vektor-slika]
 (let [refs (atom (sorted-set))
       pool (Executors/newFixedThreadPool 5)
       tasks (tasks-fn vektor-slika refs)]
  (doseq [future (.invokeAll pool tasks)]
   (.get future)
   )
  (.shutdown pool)
;  (println tasks)
;  (println @refs)
  (doseq [[thread-number red] @refs]
   (println red)
   )
;  (deref refs)
;  (doseq [red (deref refs)]
;   (println red)
;   )
  ))

(defn oblast-reda
 ""
 [y-axis-vektor]
 (let [oblasti-reda (atom [])
       oblast-reda (atom [])]
  (doseq [y y-axis-vektor]
   (when-let [[start end] @oblast-reda]
    (if (= start
           nil)
     (reset! oblast-reda [y])
     (if (= (inc start)
            y)
      (reset! oblast-reda [y])
      (do
       (when-not (< (- y
                       start)
                    50)
        (swap! oblasti-reda conj [start y])
        (reset! oblast-reda [y]))
       )
      ))
    ))
  @oblasti-reda))

(defn odredi-oblast-reda
 ""
 [image
  width
  height]
 (try
  (let [y (atom 0)
        white-rows (atom [])]
   (while (< @y height)
    (let [x (atom 0)]
     (while (< @x width)
      (let [p (.getRGB image @x @y)
            a (bit-and (bit-shift-right p 24) ; transparency
                       127)
            r (bit-and (bit-shift-right p 16) ; red
                       255)
            g (bit-and (bit-shift-right p 8) ; green
                       255)
            b (bit-and (bit-shift-right p 0) ; blue
                       255)]
       (if (and (= r 255)
                (= g 255)
                (= b 255))
        (swap! x inc)
        (swap! x + 1 width))
       ))
      (when-not (< width @x)
       ;(println @y)
       (swap! white-rows conj @y))
     )
     (swap! y inc))
   (oblast-reda @white-rows))
  (catch Exception e))
 )

(defn napravi-red
 ""
 [image
  width
  start
  end]
 (let [img (BufferedImage. width
                           (- end start)
                           (BufferedImage/TYPE_INT_ARGB))]
  (doseq [x (range width)]
   (doseq [y (range start end)]
    (let [p (.getRGB image x y)]
     (.setRGB img x (- y start) p)
     )
    )
   )
  img))

(defn nadji-min-i-max
 ""
 [prve-tacke]
 (reduce
  (fn
   [{x-min :x-min
     x-max :x-max
     y-min :y-min
     y-max :y-max}
    [x y]]
   {:x-min (if (< x x-min)
            x
            x-min)
    :x-max (if (> x x-max)
            x
            x-min)
    :y-min (if (< y y-min)
            y
            y-min)
    :y-max (if (> y y-max)
            y
            y-max)})
  {:x-min (Integer/MAX_VALUE)
   :x-max 0
   :y-min (Integer/MAX_VALUE)
   :y-max 0}
  prve-tacke))

(defn odredi-oblast-reda-ii
 ""
 [image
  width
  height]
 (try
  (let [y (atom 0)
        prve-crne-tacke (atom (sorted-set))]
   (while (< @y 56)
    (let [x (atom 0)]
     (while (< @x 56)
      (let [p (.getRGB image @x @y)
            a (bit-and (bit-shift-right p 24) ; transparency
                       127)
            r (bit-and (bit-shift-right p 16) ; red
                       255)
            g (bit-and (bit-shift-right p 8) ; green
                       255)
            b (bit-and (bit-shift-right p 0) ; blue
                       255)]
       (if (and (= r 0)
                (= g 0)
                (= b 0))
        (do
         (swap! prve-crne-tacke conj [@x @y])
         (swap! x + 1 width))
        (swap! x inc))
       ))
     )
     (swap! y inc))
   (nadji-min-i-max @prve-crne-tacke))
  (catch Exception e))
 )

(defn oboj-crveno
 ""
 [image
  x
  y]
 (let [p (.getRGB image x y)
       a (bit-and (bit-shift-right p 24) ; transparency
                  127)
       r (bit-and (bit-shift-right p 16) ; red
                  255)
       g (bit-and (bit-shift-right p 8) ; green
                  255)
       b (bit-and (bit-shift-right p 0) ; blue
                  255)
       a (bit-shift-left a 24) ; transparency
       r (bit-shift-left 255 16) ; red
       g (bit-shift-left 0 8) ; green
       b (bit-shift-left 0 0) ; blue
       p (bit-or a r g b)]
  (.setRGB image x y p)
  )
 )

(defn gornja-granicna-vrednost
 ""
 [image width y-min]
 (let [x (atom 0)
       granicne-tacke (atom #{})
       y-min (atom y-min)
       gornja-granica (if (neg? (- @y-min 30))
                       0
                       (- @y-min 30))]
  (while (< @x width)
   (let [y (atom @y-min)]
    (while (> @y gornja-granica)
     (when-not (tacka-je-crna? image @x @y)
      (oboj-crveno image @x @y)
      (swap! granicne-tacke conj [@x @y])
      (reset! y-min @y)
      (reset! y gornja-granica))
     (swap! y dec))
    )
   (swap! x inc))
  @granicne-tacke))

(defn donja-granicna-vrednost
 ""
 [image width height y-max]
 (let [x (atom 0)
       granicne-tacke (atom #{})
       y-max (atom y-max)
       donja-granica (if (< (+ @y-max 30)
                            height)
                      (+ @y-max 30)
                      height)]
  (while (< @x width)
   (let [y (atom @y-max)]
    (while (< @y donja-granica)
     (when-not (tacka-je-crna? image @x @y)
      (oboj-crveno image @x @y)
      (swap! granicne-tacke conj [@x @y])
      (reset! y-max @y)
      (reset! y donja-granica))
     (swap! y inc))
    )
   (swap! x inc))
  @granicne-tacke))

(defn granicne-vrednosti
 ""
 [image
  width
  height
  {x-min :x-min
   x-max :x-max
   y-min :y-min
   y-max :y-max}]
 (gornja-granicna-vrednost image width y-min)
 (donja-granicna-vrednost image width height y-max))

(defn napravi-vektor-slika
 ""
 [url
  width
  height]
 (try
   (let [fin (resource url)
         fout (file "resources/tekst-crveno.jpg")
         ]
    (if (nil? fin)
     (println "file not found or there was a problem reading it")
     (let [image (BufferedImage. width
                                 height
                                 (BufferedImage/TYPE_INT_ARGB))
           image (ImageIO/read fin)
           width (.getWidth image)
           height (.getHeight image)]
      (grayscale-contrast image width height)
      (obrisi-piksele image width height)
      (let [redovi (odredi-oblast-reda image width height)
            images (atom [])]
       (doseq [[start end] redovi]
        (swap! images conj [(napravi-red image width start end) width (- end start)])
        )
       @images)
      )
    ))
   (catch IOException e
    (println "greska")
    (println (.getMessage e))
    ))
 )

(defn napravi-vektor-slika-ii
 ""
 [url
  width
  height]
 (try
   (let [fin (resource url)
         fout (file "resources/tekst-crveno.jpg")
         ]
    (if (nil? fin)
     (println "file not found or there was a problem reading it")
     (let [image (BufferedImage. width
                                 height
                                 (BufferedImage/TYPE_INT_ARGB))
           image (ImageIO/read fin)
           width (.getWidth image)
           height (.getHeight image)]
      (grayscale-contrast image width height)
      (obrisi-piksele image width height)
      (let [x-y-min-max (odredi-oblast-reda-ii image width height)]
       (granicne-vrednosti image width height x-y-min-max)
       )
      (ImageIO/write image "jpg" fout)
      )
    ))
   (catch IOException e
    (println "greska")
    (println (.getMessage e))
    ))
 )

(defn procitaj-sliku
 ""
 [url
  width
  height
  & [sacuvaj-sliku]]
 (try
   (let [fin (resource url)
         ;fout (file "resources/dostojevski/obradjena_slika.jpg")
         ]
    (if (nil? fin)
     (println "file not found or there was a problem reading it")
     (let [image (BufferedImage. width
                                 height
                                 (BufferedImage/TYPE_INT_ARGB))
           image (ImageIO/read fin)
           width (.getWidth image)
           height (.getHeight image)]
      ;(println "grayscale-contrast")
      (grayscale-contrast image width height 38)
      ;(println "obrisi-piksele")
      ;(time (obrisi-piksele image width height))
      ;(when sacuvaj-sliku
      ; (ImageIO/write image "jpg" fout))
      image))
    )
   (catch IOException e
    (println "greska")
    (println (.getMessage e))
    ))
 )

(defn nadji-ostale-crne-tacke
 ""
 [image
  coll-set
  acc
  width
  height]
  (if (empty? coll-set)
   acc
   (let [[x y] (first coll-set)
         coll-set (disj coll-set [x y])
         koord (koordinate-okoline-skup x y)
         koord (reduce
                (fn
                 [acc
                  [x y]]
                 (if (and (< x width)
                          (> x -1)
                          (< y height)
                          (> y -1)
                          (tacka-je-crna? image x y))
                  (conj acc [x y])
                  acc))
                 #{}
                 koord)
         diff-coll-set (difference koord coll-set)
         diff-acc (difference diff-coll-set acc)
         coll-set (reduce conj coll-set diff-acc)]
    (recur image coll-set (conj acc [x y]) width height)
    ))
  )

(defn proveri-kolonu
 ""
 [image
  height
  x
  y-vektor
  index]
 (if (< index (count y-vektor))
  (let [y (get y-vektor index)]
   (if (< y height)
    (if (tacka-je-crna? image x y)
     [x y]
     (recur image height x y-vektor (inc index)))
    nil))
  nil))

(defn nadji-prvu-crnu-tacku
 ""
 [image
  width
  height
  x
  y-vektor]
  ;(println [x y-vektor])
  (if (< x width)
   (let [prva-tacka (proveri-kolonu image height x y-vektor 0)]
    (if-let [[x-t y-t] prva-tacka]
     [x-t y-t true]
     (recur image width height (inc x) y-vektor))
    )
   [x (first y-vektor) false]))

(defn odredi-nepoznato-slovo
 ""
 [stns]
 (let [stns-na-gore (pomeri-skup-tacaka
                     stns
                     0
                     -1)
       stns-u-levo (pomeri-skup-tacaka
                    stns
                    -1
                    0)
       stns-u-levo-i-na-gore (pomeri-skup-tacaka
                              stns
                              -1
                              -1)
;       test-a (println stns)
       poklapanja-slova (atom [])]
  (doseq [[tacke slovo sirina visina] @slova]
   (let [p0 (count (intersection tacke
                                 stns))
         r0 (count (difference tacke
                               stns))
         p1 (count (intersection tacke
                                 stns-na-gore))
         r1 (count (difference tacke
                               stns-na-gore))
         p2 (count (intersection tacke
                                 stns-u-levo))
         r2 (count (difference tacke
                               stns-u-levo))
         p3 (count (intersection tacke
                                 stns-u-levo-i-na-gore))
         r3 (count (difference tacke
                               stns-u-levo-i-na-gore))
         poklopljene-tacke (int (/ (+ p0 p1 p2 p3) 4))
         ne-poklopljene-tacke (int (/ (+ r0 r1 r2 r3) 4))
         ;broj-tacaka-slova (count tacke)
         ;broj-tacaka-nepoznatog-slova (count vektor-nepoznatog-slova)
         ]
    ;(println (char slovo) " " p0 " " r0 " " p1 " " r1 " " p2 " " r2 " " p3 " " r3 " " broj-tacaka-slova " " broj-tacaka-nepoznatog-slova)
    ;(println (char slovo) " " poklopljene-tacke " " ne-poklopljene-tacke " " broj-tacaka-slova " " broj-tacaka-nepoznatog-slova)
    (swap! poklapanja-slova conj
     {:slovo (char slovo)
      :broj-poklopljenih-tacaka poklopljene-tacke
      :razlika ne-poklopljene-tacke}))
   )
  ;(println @poklapanja-slova)
  (:slovo (most-probable-vektor @poklapanja-slova))
  )
 )

(defn unija-svih
 ""
 [vektor-tacaka-redova]
 (let [skup-svih-tacaka (atom nil)]
  (doseq [red vektor-tacaka-redova]
   (doseq [skup-tacaka-slova red]
    (swap! skup-svih-tacaka union skup-tacaka-slova)))
  @skup-svih-tacaka))

(defn nadji-crne-tacke
 ""
 [image
  width
  height]
 (let [y-a (atom 0)
       vektor-tacaka-redova (atom [])
       vektor-skupova-tacaka-slova-po-redovima (atom [])
       y-max-prvog-slova-u-redu (atom 0)]
  (while (< @y-a height)
   (let [x-a (atom 0)
         prva-iteracija (atom true)
         min-max (atom {:x-max 0
                        :y-min (Integer/MAX_VALUE)
                        :x-min (Integer/MAX_VALUE)
                        :y-max 0})
         x-max-prethodni (atom 0)]
    (while (< @x-a width)
     (println [@x-a @y-a @y-max-prvog-slova-u-redu])
     (let [max-half (int (/ @max-height 4))
           y-vektor (vec (if (= @y-max-prvog-slova-u-redu
                                0)
                          (range @y-a
                                 (+ @y-a
                                    @max-height))
                          (range (+ @y-max-prvog-slova-u-redu
                                    max-half)
                                 (- @y-max-prvog-slova-u-redu
                                    max-half) -1))
                     )]
      (let [[x y indikator] (nadji-prvu-crnu-tacku image
                                                   width
                                                   height
                                                   @x-a
                                                   y-vektor)]
       (if indikator
        (when-not (contains? (apply union
                              (apply union
                               (conj @vektor-tacaka-redova
                                     @vektor-skupova-tacaka-slova-po-redovima))
                              )
                            [x y])
         (reset! x-a x)
         (reset! y-a y)
         (let [tacke-slova (nadji-ostale-crne-tacke image #{[x y]} #{} width height)]
          (when (and tacke-slova
                     (< (- @min-dots
                           20)
                        (count tacke-slova))
                     (< (count tacke-slova)
                        (* @max-dots
                           2))
                 )
           (let [tacke-slova (atom tacke-slova)]
            (when (< razmak
                     (- x
                        @x-max-prethodni))
             (print \space))
            (doseq [[x y] @tacke-slova]
             (when (< x (:x-min @min-max))
              (swap! min-max assoc :x-min x))
             (when (< y (:y-min @min-max))
              (swap! min-max assoc :y-min y))
             (when (< (:x-max @min-max) x)
              (swap! min-max assoc :x-max x))
             (when (< (:y-max @min-max) y)
              (swap! min-max assoc :y-max y))
             )
            (let [x-min (:x-min @min-max)
                  x-max (:x-max @min-max)
                  y-min (- (:y-min @min-max)
                           10)
                  y-max (- (:y-min @min-max)
                           5)]
             ;(println [x-min x-max y-min y-max])
             (let [[x-kt y-kt indikator] (nadji-prvu-crnu-tacku image
                                                                x-max
                                                                height
                                                                x-min
                                                                (vec
                                                                 (range y-min y-max))
                                          )]
              (when indikator
               (when-not (contains? (apply union
                                     (apply union
                                      (conj @vektor-tacaka-redova
                                            @vektor-skupova-tacaka-slova-po-redovima))
                                     )
                                   [x-kt y-kt])
                (when-let [tacke-kukice (nadji-ostale-crne-tacke image #{[x-kt y-kt]} #{} width height)]
                 (if (< min-tacke-kukice
                        (count tacke-kukice))
                  (swap!
                   tacke-slova
                   (fn [t-s
                        t-k]
                    (reduce
                     conj
                     t-s
                     t-k))
                    tacke-kukice))
                 ))
               ))
             )
            (swap! vektor-skupova-tacaka-slova-po-redovima conj @tacke-slova)
            ((fn []
              (reset! min-max {:x-max 0
                               :y-min (Integer/MAX_VALUE)
                               :x-min (Integer/MAX_VALUE)
                               :y-max 0})
              (doseq [[x y] @tacke-slova]
               (when (< x (:x-min @min-max))
                (swap! min-max assoc :x-min x))
               (when (< y (:y-min @min-max))
                (swap! min-max assoc :y-min y))
               (when (< (:x-max @min-max) x)
                (swap! min-max assoc :x-max x))
               (when (< (:y-max @min-max) y)
                (swap! min-max assoc :y-max y))
               )
              (when (< @max-width
                       (- (:x-max @min-max)
                          (:x-min @min-max)))
               (let [procitano-slovo (odredi-nepoznato-slovo
                                      (svedi-na-nulte-koordinate
                                       @tacke-slova))
                     sirina-prvog ((fn [index]
                                    (when-let [[_ slovo width _] (get @slova index)]
                                     (if (= (int procitano-slovo)
                                            slovo)
                                      width
                                      (recur (inc index))
                                      ))
                                    )
                                   0)]
                (print procitano-slovo)
                (reset! tacke-slova (reduce
                                     (fn [acc
                                          [x y]]
                                      (if (< (+ (:x-min @min-max)
                                                sirina-prvog)
                                             x)
                                       (conj acc [x y])
                                       acc))
                                     #{}
                                     @tacke-slova))
                )
               (recur))
              ))
            (print
             (odredi-nepoznato-slovo
              (svedi-na-nulte-koordinate
               @tacke-slova)))
            (reset! x-a (:x-max @min-max))
            (reset! x-max-prethodni @x-a)
            (reset! y-a (:y-max @min-max))
            (when @prva-iteracija
             (reset! y-max-prvog-slova-u-redu (:y-max @min-max)))
            (reset! prva-iteracija false))
           ))
         )
        (do
         (reset! x-a x)
         (reset! y-a y)
         ))
       ))
     (reset! min-max {:x-max 0
                      :y-min (Integer/MAX_VALUE)
                      :x-min (Integer/MAX_VALUE)
                      :y-max 0})
     (swap! x-a inc))
    )
    (when (< @y-max-prvog-slova-u-redu @y-a)
     (reset! y-a @y-max-prvog-slova-u-redu))
    (when (< height
             (+ @y-a
                @max-height))
     (reset! y-a height))
    (swap! y-a inc)
    (reset! y-max-prvog-slova-u-redu 0)
    (swap! vektor-tacaka-redova conj @vektor-skupova-tacaka-slova-po-redovima)
    (reset! vektor-skupova-tacaka-slova-po-redovima [])
    (println)
   )
  )
 )

(defn nadji-tacke-znaka
 ""
 [sve_tacke
  tacke_znaka
  proverene_tacke
  width
  height]
 (if (= (difference
         tacke_znaka
         proverene_tacke)
        #{})
  [sve_tacke tacke_znaka]
  (let [ne_proverene_tacke (difference
                            tacke_znaka
                            proverene_tacke)
        [x y] (first ne_proverene_tacke)
        koord (koordinate-okoline-skup x y)
        koord (reduce
                (fn
                 [acc
                  [x y]]
                 (if (and (< x width)
                          (> x -1)
                          (< y height)
                          (> y -1)
                          (contains? sve_tacke [x y]))
                  (conj acc [x y])
                  acc))
                 #{}
                 koord)
        sve_tacke (apply disj sve_tacke koord)
        tacke_znaka (apply conj tacke_znaka koord)
        proverene_tacke (conj proverene_tacke [x y])]
   (recur sve_tacke tacke_znaka proverene_tacke width height))
  ))

(defn kvacica
 ""
 [sve_tacke
  x
  y-range]
  (if (empty? y-range)
   [-1 -1]
   (let [y-range-el (first y-range)]
    (if (contains? sve_tacke [x y-range-el])
     [x y-range-el]
     (recur sve_tacke x (rest y-range))
     ))
   ))

(defn tacke-i-kvacice
 ""
 [sve_tacke
  x-min
  x-max
  y-min
  y-max]
 (let [x-middle (+ x-min (int (/ (- x-max x-min) 2)))
       y-slova (reduce
                (fn [acc el]
                 (conj acc
                       (+ (first acc)
                          el))
                 )
                [y-max]
                (range 1 8))
       y-kvacice (reduce
                  (fn [acc el]
                   (conj acc
                         (- (first acc)
                            el))
                   )
                  [y-min]
                  (range 1 8))
       [x-slova y-slova] (kvacica
                           sve_tacke
                           x-middle
                           y-slova)
       [x-kvacice y-kvacice] (kvacica
                               sve_tacke
                               x-middle
                               y-kvacice)]
  
  (if (contains? sve_tacke [x-slova y-slova])
   [x-slova y-slova]
   (if (contains? sve_tacke [x-kvacice y-kvacice])
    [x-kvacice y-kvacice]
    [-1 -1]))
  ))

(defn grupisanje
 ""
 [sve_tacke
  skupovi_tacaka_slova
  width
  height]
 (if (empty? sve_tacke)
  skupovi_tacaka_slova
  (let [[x y] (first sve_tacke)
        sve_tacke (disj sve_tacke [x y])
        [sve_tacke_nove tacke_znaka]
         (nadji-tacke-znaka
           sve_tacke
           #{[x y]}
           #{}
           width
           height)
        [x-min y-min x-max y-max]
         (find-min-max
           tacke_znaka)
        [x-kvacice y-kvacice]
         (tacke-i-kvacice
           sve_tacke_nove
           x-min
           x-max
           y-min
           y-max)
        [sve_tacke_nove tacke_znaka]
         (if (not= x-kvacice -1)
          (nadji-tacke-znaka sve_tacke_nove
                             (conj tacke_znaka
                                   [x-kvacice y-kvacice])
                             tacke_znaka
                             width
                             height)
          [sve_tacke_nove tacke_znaka])
        [x-min y-min x-max y-max] (find-min-max tacke_znaka)
        skupovi_tacaka_slova (conj skupovi_tacaka_slova
                                   [[x-min y-min x-max y-max] tacke_znaka])]
    ;(println (count sve_tacke_nove))
    (recur sve_tacke_nove skupovi_tacaka_slova width height)
    ;(if (= (count skupovi_tacaka_slova) 10)
    ; skupovi_tacaka_slova
     
    ; )
    ))
 )

(defn procitaj-crne-tacke
 ""
 [image
  width
  height]
 (let [sve_tacke (atom #{})]
  (doseq [x (range 0 width)]
   (doseq [y (range 0 height)]
    (when (tacka-je-crna? image x y)
     (swap! sve_tacke conj [x y])
     ))
   )
  @sve_tacke))

(defn nacrtaj-znak
 ""
 [img
  koord]
 (let [width (.getWidth img)
       height (.getHeight img)]
  (doseq [x (range width)]
   (doseq [y (range height)]
    (let [a (bit-shift-left 127 24) ; transparency
          r (bit-shift-left 255 16) ; red
          g (bit-shift-left 255 8) ; green
          b (bit-shift-left 255 0) ; blue
          p (bit-or a r g b)]
     (.setRGB img x y p))
    )
   )
  )
 (doseq [[x y] koord]
  (let [a (bit-shift-left 127 24) ; transparency
        r (bit-shift-left 0 16) ; red
        g (bit-shift-left 0 8) ; green
        b (bit-shift-left 0 0) ; blue
        p (bit-or a r g b)]
   (.setRGB img x y p))
  )
 img)

(defn poklapanje-znaka
 ""
 [tacke_znaka]
 (let [tacke_znaka_na_gore (pomeri-skup-tacaka
                            tacke_znaka
                            0
                            -1)
       tacke_znaka_u_levo (pomeri-skup-tacaka
                           tacke_znaka
                           -1
                           0)
       tacke_znaka_u_levo_i_na_gore (pomeri-skup-tacaka
                                     tacke_znaka
                                     -1
                                     -1)
       poklapanja-slova (atom [])]
  (doseq [[tacke slovo sirina visina] @slova]
   (let [p0 (count (intersection tacke
                                 tacke_znaka))
         r0 (count (difference tacke
                               tacke_znaka))
         p1 (count (intersection tacke
                                 tacke_znaka_na_gore))
         r1 (count (difference tacke
                               tacke_znaka_na_gore))
         p2 (count (intersection tacke
                                 tacke_znaka_u_levo))
         r2 (count (difference tacke
                               tacke_znaka_u_levo))
         p3 (count (intersection tacke
                                 tacke_znaka_u_levo_i_na_gore))
         r3 (count (difference tacke
                               tacke_znaka_u_levo_i_na_gore))
         poklopljene-tacke (int (/ (+ p0 p1 p2 p3) 4))
         ne-poklopljene-tacke (int (/ (+ r0 r1 r2 r3) 4))
         ;broj-tacaka-slova (count tacke)
         ;broj-tacaka-nepoznatog-slova (count vektor-nepoznatog-slova)
         ]
    ;(println (char slovo) " " p0 " " r0 " " p1 " " r1 " " p2 " " r2 " " p3 " " r3 " " broj-tacaka-slova " " broj-tacaka-nepoznatog-slova)
    ;(println (char slovo) " " poklopljene-tacke " " ne-poklopljene-tacke " " broj-tacaka-slova " " broj-tacaka-nepoznatog-slova)
    (swap! poklapanja-slova conj
     {:slovo (char slovo)
      :broj-poklopljenih-tacaka poklopljene-tacke
      :razlika ne-poklopljene-tacke}))
   )
  ;(println @poklapanja-slova)
  (:slovo (most-probable-vektor @poklapanja-slova))
  ))

(defn procitaj-tekst
 ""
 []
 (let [tekst ["tekst.jpg" 3096 4128]
       out ["out.jpg" 3096 4128]
       out1 ["out1.jpg" 3096 4128]
       out2 ["out2.jpg" 2464 2216]
       out3 ["out3.jpg" 2464 2216]
       out4 ["out4.jpg" 2464 50]
       out5 ["out5.jpg" 90 34]
       out6 ["out6.jpg" 90 34]
       out7 ["out7.jpg" 150 34]
       out8 ["out8.jpg" 150 34]
       out9 ["out9.jpg" 161 44]
       out10 ["out10.jpg" 1489 80]
       out10-a ["out10-a.jpg" 1489 80]
       out11 ["out11.jpg" 37 41]
       red1 ["red1.jpg" 1731 96]
       red2 ["red2.jpg" 326 67]
       red3 ["red3.jpg" 963 85]
       red4 ["red4.jpg" 2032 96]
       red5 ["red5.jpg" 1902 92]
       tekst1 ["tekst1.jpg" 2032 450]
       tekst2 ["tekst2.jpg" 1496 330]
       tekst3 ["tekst3.jpg" 1516 344]
       tekst4 ["tekst4.jpg" 1518 336]
       tekst5 ["tekst5.jpg" 1515 338]
       tekst6 ["tekst6.jpg" 1848 1816]
       slovo-o ["o.jpg" 28 29]
       dostojevski_tekst ["dostojevski/tekst.jpg" 1536 2560]
       dostojevski_tekst1 ["dostojevski/tekst1.jpg" 1461 352]
       dostojevski_i ["dostojevski/i.jpg" 28 45]
       dostojevski_sh ["dostojevski/sh.jpg" 25 45]
       [url width height] dostojevski_tekst1]
  ;(procitaj-sliku url width height true)
  ;(let [image (procitaj-sliku url width height)]
  ; (nadji-crne-tacke image width height))
  (let [image (time (procitaj-sliku url width height))
        sve_tacke (time (procitaj-crne-tacke image width height))
        svi_znakovi (time
                     (grupisanje
                      sve_tacke
                      (sorted-set-by
                       (fn
                        [[[x-min1 y-min1 x-max1 y-max1] elem1]
                         [[x-min2 y-min2 x-max2 y-max2] elem2]]
                        (let [srednja-tacka-y1 (+ y-min1 (/ (- y-max1 y-min1) 2))
                              srednja-tacka-y2 (+ y-min2 (/ (- y-max2 y-min2) 2))]
                         (if (or (< y-min2 srednja-tacka-y1 y-max2)
                                 (< y-min1 srednja-tacka-y2 y-max1))
                          (< x-min1 x-min2)
                          (< y-min1 y-min2))
                         ))
                       )
                      width
                      height))
        itr (atom 0)]
   (time (doseq [[_ tacke_znaka] svi_znakovi]
          #_(let [nulte_koordinate (svedi-na-nulte-koordinate tacke_znaka)]
           (println (poklapanje-znaka nulte_koordinate))
           )
          (try
           (let [nulte-koordinate (svedi-na-nulte-koordinate tacke_znaka)
                 fout (file (str "resources/dostojevski/out/znak" @itr ".jpg"))
                 [_ _ x-max y-max] (find-min-max nulte-koordinate)
                 width (inc x-max)
                 height (inc y-max)
                 img (.getSubimage image 0 0 width height)]
            (nacrtaj-znak img nulte-koordinate)
            (ImageIO/write img "jpg" fout)
            (swap! itr inc))
           (catch IOException e
            (println "greska")
            (println (.getMessage e))
            ))
          ))
   )
  ;(napravi-vektor-slika-ii tekst2 width height)
  ;(multi-tasking-slika (napravi-vektor-slika tekst2 width height))
  ;(rw-image url width height "procitaj-tekst")
  ;(multi-tasking [
  ;                red1
  ;                red2
  ;                red3
  ;                red4
  ;                red5
  ;                ])
  )
 )

(defn nauci-slova
 ""
 []
 (let [dostojevski_slova_dir_name "dostojevski/slova/"
       dostojevski_slova [["3.jpg" 23 39 \3]
                          ["6.jpg" 22 39 \6]
                          ["9.jpg" 22 39 \9]
                          ["a.jpg" 28 29 \а]
                          ["A.jpg" 37 37 \А]
                          ["b.jpg" 30 38 \б]
                          ["B.jpg" 39 38 \Б]
                          ["c.jpg" 23 29 \ц]
                          ["ch.jpg" 25 39 \ч]
                          ["cj.jpg" 25 39 \ћ]
                          ["colon.jpg" 7 27 \:]
                          ["comma.jpg" 9 15 \,]
                          ["d.jpg" 31 39 \д]
                          ["D.jpg" 40 38 \Д]
                          ["dash.jpg" 56 5 \-]
                          ["dash-1.jpg" 12 6 \-]
                          ["dj.jpg" 30 38 \ђ]
                          ["dot.jpg" 6 7 \.]
                          ["e.jpg" 24 29 \е]
                          ["E.jpg" 24 25 \Е]
                          ["exclamation_mark.jpg" 8 39 \!]
                          ["F.jpg" 22 25 \Ф]
                          ["g.jpg" 26 38 \г]
                          ["greater_then.jpg" 10 27 \>]
                          ["greater_then_1.jpg" 9 26 \>]
                          ["h.jpg" 32 38 \х]
                          ["i.jpg" 15 39 \и]
                          ["I.jpg" 21 38 \И]
                          ["j.jpg" 15 49 \ј]
                          ["J.jpg" 26 39 \ј]
                          ["k.jpg" 32 39 \к]
                          ["K.jpg" 44 37 \К]
                          ["l.jpg" 15 38 \л]
                          ["less_then.jpg" 10 26 \<]
                          ["less_then_1.jpg" 9 25 \<]
                          ["m.jpg" 50 29 \м]
                          ["M.jpg" 49 37 \М]
                          ["n.jpg" 32 29 \н]
                          ["N.jpg" 41 38 \Н]
                          ["o.jpg" 28 30 \о]
                          ["O.jpg" 39 40 \О]
                          ["p.jpg" 31 38 \п]
                          ["parentheses_close.jpg" 14 40 \)]
                          ["parentheses_open.jpg" 13 40 \(]
                          ["question_mark.jpg" 20 38 \?]
                          ["r.jpg" 23 28 \р]
                          ["s.jpg" 22 29 \с]
                          ["S.jpg" 31 39 \С]
                          ["semicolon.jpg" 8 36 \;
                           ]
                          ["sh.jpg" 21 39 \ш]
                          ["t.jpg" 19 38 \т]
                          ["u.jpg" 32 29 \у]
                          ["U.jpg" 39 38 \У]
                          ["v.jpg" 29 29 \в]
                          ["V.jpg" 38 38 \В]
                          ["z.jpg" 24 28 \з]
                          ["Z.jpg" 32 39 \З]
                          ["zj.jpg" 32 39 \ж]]
       slike-slova [["0.jpg" 45 56 \0]
                    ["1.jpg" 27 40 \1]
                    ["2.jpg" 29 40 \2]
                    ["3.jpg" 27 48 \3]
                    ["4.jpg" 34 41 \4]
                    ["5.jpg" 30 50 \5]
                    ["6.jpg" 30 41 \6]
                    ["7.jpg" 30 49 \7]
                    ["8.jpg" 30 40 \8]
                    ["9.jpg" 30 48 \9]
                    ["a.jpg" 27 28 \а]
                    ["A.jpg" 36 39 \А]
                    ["b.jpg" 30 38 \б]
                    ["b-lat.jpg" 33 41 \b]
                    ["c.jpg" 26 29 \ц]
                    ["C.jpg" 31 39 \Ц]
                    ["ch.jpg" 26 45 \ч]
                    ["cj.jpg" 27 43 \ћ]
                    ["comma.jpg" 13 21 \,]
                    ["d.jpg" 31 39 \д]
                    ["dot.jpg" 16 14 \.]
                    ["e.jpg" 27 28 \е]
                    ["E.jpg" 35 42 \Е]
                    ["equals.jpg" 27 16 \=]
                    ["f.jpg" 32 40 \ф]
                    ["F.jpg" 35 40 \Ф]
                    ["g.jpg" 33 40 \г]
                    ["G.jpg" 31 40 \Г]
                    ["h.jpg" 33 40 \х]
                    ["i.jpg" 29 40 \и]
                    ["I.jpg" 29 39 \И]
                    ["j.jpg" 23 48 \ј]
                    ["J.jpg" 32 40 \ј]
                    ["k.jpg" 33 39 \к]
                    ["K.jpg" 37 40 \К]
                    ["l.jpg" 28 40 \л]
                    ["L.jpg" 38 40 \Л]
                    ["m.jpg" 32 29 \м]
                    ["M.jpg" 33 39 \М]
                    ["m-lat.jpg" 34 30 \m]
                    ["minus.jpg" 34 9 \-]
                    ["n.jpg" 33 29 \н]
                    ["N.jpg" 34 40 \Н]
                    ["o.jpg" 28 29 \о]
                    ["O.jpg" 36 41 \О]
                    ["p.jpg" 31 38 \п]
                    ["P.jpg" 39 42 \П]
                    ["parentheses_close.jpg" 19 55 \)]
                    ["parentheses_open.jpg" 19 54 \(]
                    ;["quote.jpg" 26 24 \"]
                    ["r.jpg" 31 28 \р]
                    ["R.jpg" 35 41 \Р]
                    ["s.jpg" 26 30 \с]
                    ["S.jpg" 35 41 \С]
                    ["slash.jpg" 39 53 \/]
                    ["sh.jpg" 25 42 \ш]
                    ["t.jpg" 29 38 \т]
                    ["T.jpg" 32 38 \Т]
                    ["u.jpg" 32 31 \у]
                    ["U.jpg" 35 39 \У]
                    ["v.jpg" 33 28 \в]
                    ["W.jpg" 35 41 \W]
                    ["z.jpg" 27 28 \з]
                    ["zj.jpg" 24 45 \ж]]
       dir-name dostojevski_slova_dir_name;nil
       ]
  (doseq [[url
           width
           height
           slovo] dostojevski_slova]
   (rw-image
    (str dir-name
         url)
    width
    height
    "procitaj-slovo"
    slovo)
   (when (< width @min-width)
    (reset! min-width width))
   (when (< @max-width width)
    (reset! max-width width))
   (when (< height @min-height)
    (reset! min-height height))
   (when (< @max-height height)
    (reset! max-height height))
   (when-let [[tacke _ _ _] (last @slova)]
    (when (< @max-dots (count tacke))
     (reset! max-dots (count tacke))
     )
    (when (< (count tacke) @min-dots)
     (reset! min-dots (count tacke))
     ))
   ))
 )

(nauci-slova)

