(ns ocr.core
 (:require [clojure.java.io :refer [resource file writer]]
           [clojure.set :refer [intersection difference]])
 (:import [java.io File
                   IOException]
          [java.awt.image BufferedImage]
          [javax.imageio ImageIO]
          [java.util.concurrent Executors]))

(def slova (atom []))

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
    (if (< color 0)
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

(defn find-mins
 ""
 [vektor-tacaka]
 (let [x-min (atom (Integer/MAX_VALUE))
       y-min (atom (Integer/MAX_VALUE))]
  (doseq [[x y] vektor-tacaka]
   (when (< x @x-min)
    (reset! x-min x))
   (when (< y @y-min)
    (reset! y-min y))
   )
;  (println [@x-min @y-min])
  [@x-min @y-min]))

(defn svedi-na-nulte-koordinate
 ""
 [vektor-tacaka]
 (let [[x-min y-min] (find-mins vektor-tacaka)
       novi-vektor-tacaka (atom #{})]
  (doseq [[x y] vektor-tacaka]
   (swap! novi-vektor-tacaka conj [(- x x-min) (- y y-min)]))
  @novi-vektor-tacaka))

(defn procitaj-nepoznato-slovo
 ""
 [image
  [start end]
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
  height]
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
         avg (int (/ (+ r g b) 3))
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
    )
   )
  )
; (println "kontrast")
; (println width)
; (println height)
; (println @slova)
 )

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

(defn citaj
 ""
 [image
  height
  ob-slova
  thread-number]
 (let [tekst (atom "")
       prethodni-end (atom 0)]
  (doseq [[start end] ob-slova]
   (let [vektor-nepoznatog-slova (procitaj-nepoznato-slovo image [start end] height)
;         test-a (println vektor-nepoznatog-slova)
         poklapanja-slova (atom [])]
    (doseq [[tacke slovo sirina visina] @slova]
     (let [poklopljene-tacke (count (intersection tacke
                                                  vektor-nepoznatog-slova))
           ne-poklopljene-tacke (count (difference tacke
                                                   vektor-nepoznatog-slova))]
      (swap! poklapanja-slova conj
       {:slovo (char slovo)
        :broj-poklopljenih-tacaka poklopljene-tacke
        :razlika ne-poklopljene-tacke}))
     )
           ;(println @poklapanja-slova)
;           (println (most-probable-vektor @poklapanja-slova))
    (when (< 16
             (- start
                @prethodni-end))
     (swap! tekst str " "))
    (reset! prethodni-end end)
    (swap! tekst str (:slovo (most-probable-vektor @poklapanja-slova))
     )
;     (alter refs str (:slovo (most-probable-vektor @poklapanja-slova))
;      )
    )
   )
  [thread-number @tekst])
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
       (reset! oblast-slova [])
       (swap! oblasti-slova conj [start x]))
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
         (alter refs conj (rw-image url width height "procitaj-tekst" thread-number))
         )
        )
      )
     vektor-slika
     (range (count vektor-slika))
  ))

(defn multi-tasking
 [vektor-slika]
 (let [refs (ref (sorted-set))
       pool (Executors/newFixedThreadPool 5)
       tasks (tasks-fn vektor-slika refs)]
  (doseq [future (.invokeAll pool tasks)]
   (.get future)
   )
  (.shutdown pool)
;  (deref refs)
  (doseq [[thread-number red] (deref refs)]
   (println red)
   )
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
       red1 ["red1.jpg" 1489 80]
       red2 ["red2.jpg" 329 61]
       red3 ["red3.jpg" 954 80]
       red4 ["red4.jpg" 2048 97]
       red5 ["red5.jpg" 1900 86]]
;  (rw-image url width height "procitaj-tekst")
   (multi-tasking [
                   red1
                   red2
                   red3
                   red4
                   red5
                   ])
  )
 )

(defn nauci-slova
 ""
 []
 (let [slike-slova [["a.jpg" 31 32 \а]
                    ["d.jpg" 34 42 \д]
                    ["e.jpg" 31 32 \е]
                    ["G.jpg" 36 45 \Г]
                    ["n.jpg" 37 33 \н]
                    ["o.jpg" 33 33 \о]
                    ["u.jpg" 34 34 \у]
                    ["dot.jpg" 18 16 \.]
                    ["S.jpg" 37 43 \С]
                    ["k.jpg" 35 41 \к]
                    ["minus.jpg" 37 11 \-]
                    ["C.jpg" 33 41 \Ц]
                    ["zj.jpg" 26 47 \ж]
                    ["p.jpg" 35 42 \п]
                    ["r.jpg" 35 32 \р]
                    ["s.jpg" 30 34 \с]
                    ["t.jpg" 33 42 \т]
                    ["T.jpg" 36 43 \Т]
                    ["v.jpg" 37 32 \в]
                    ["z.jpg" 31 33 \з]
                    ["j.jpg" 27 52 \ј]
                    ["l.jpg" 32 44 \л]
                    ["1.jpg" 31 44 \1]
                    ["3.jpg" 31 52 \3]
                    ["8.jpg" 34 44 \8]
                    ["9.jpg" 34 52 \9]
                    ["i.jpg" 33 44 \и]]
       ]
  (doseq [[url
           width
           height
           slovo] slike-slova]
   (rw-image url width height "procitaj-slovo" slovo)
   )
  )
 )

(nauci-slova)

