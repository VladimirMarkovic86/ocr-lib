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

(defn koordinate-okoline
 ""
 [x y]
 [[(dec x) y]
  [(inc x) y]
  [x (dec y)]
  [x (inc y)]
  [(dec x) (inc y)]
  [(inc x) (inc y)]
  [(inc x) (dec y)]
  [(dec x) (dec y)]])

(defn proveri-okolinu
 ""
 [image
  koord
  index]
 (if (< index (count koord))
  (let [[x y] (get koord index)
        p (.getRGB image x y)
        a (bit-and (bit-shift-right p 24) ; transparency
                   127)
        r (bit-and (bit-shift-right p 16) ; red
                   255)
        g (bit-and (bit-shift-right p 8) ; green
                   255)
        b (bit-and (bit-shift-right p 0) ; blue
                   255)]
   (if (and (= r
               255)
            (= g
               255)
            (= b
               255))
    (recur image koord (inc index))
    false))
  true))

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
                   0)
                (proveri-okolinu image (koordinate-okoline x y) 0))
      (let [r 255
            g 255
            b 255
            a (bit-shift-left 0 24) ; transparency
            r (bit-shift-left r 16) ; red
            g (bit-shift-left g 8) ; green
            b (bit-shift-left b 0) ; blue
            p (bit-or a r g b)]
       (.setRGB image x y p)
       )
      )
     )
    (catch Exception e
     ;(println (.getMessage e))
     ))
   )
  )
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

(defn napravi-vektor-slika
 ""
 [url
  width
  height]
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
       [tekst1 width height] ["tekst1.jpg" 2032 450]]
  (multi-tasking-slika (napravi-vektor-slika tekst1 width height))
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
 (let [slike-slova [["1.jpg" 27 40 \1]
                    ["3.jpg" 27 48 \3]
                    ["6.jpg" 30 41 \6]
                    ["8.jpg" 30 40 \8]
                    ["9.jpg" 30 48 \9]
                    ["a.jpg" 27 28 \а]
                    ["b.jpg" 30 38 \б]
                    ["C.jpg" 31 39 \Ц]
                    ["comma.jpg" 13 21 \,]
                    ["d.jpg" 31 39 \д]
                    ["dot.jpg" 16 14 \.]
                    ["e.jpg" 27 28 \е]
                    ["g.jpg" 33 40 \г]
                    ["G.jpg" 31 40 \Г]
                    ["i.jpg" 29 40 \и]
                    ["I.jpg" 29 39 \И]
                    ["j.jpg" 23 48 \ј]
                    ["k.jpg" 33 39 \к]
                    ["l.jpg" 28 40 \л]
                    ["m.jpg" 32 29 \м]
                    ["M.jpg" 33 39 \М]
                    ["minus.jpg" 34 9 \-]
                    ["n.jpg" 33 29 \н]
                    ["o.jpg" 28 29 \о]
                    ["p.jpg" 31 38 \п]
                    ["parentheses_close.jpg" 19 55 \)]
                    ["parentheses_open.jpg" 19 54 \(]
                    ["r.jpg" 31 28 \р]
                    ["s.jpg" 26 30 \с]
                    ["S.jpg" 35 41 \С]
                    ["sh.jpg" 25 42 \ш]
                    ["t.jpg" 29 38 \т]
                    ["T.jpg" 32 38 \Т]
                    ["u.jpg" 32 31 \у]
                    ["U.jpg" 35 39 \У]
                    ["v.jpg" 33 28 \в]
                    ["z.jpg" 27 28 \з]
                    ["zj.jpg" 24 45 \ж]]
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

