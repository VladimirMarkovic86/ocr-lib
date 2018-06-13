(ns ocr-lib.threading
 (import [java.util.concurrent Executors]))

(defn test-stm [nitems nthreads niters]
  (let [refs  (map ref (repeat 10 0))
        pool  (Executors/newFixedThreadPool nthreads)
        tasks (map (fn [t]
                      (Thread/sleep 4000)
                      (fn []
                        (dotimes [n niters]
                          (dosync
                            (doseq [r refs]
                              (alter r + 1 t))
                           ))
                       ))
                   (range nthreads))]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)
    (map deref refs)))

(time (test-stm 10 10 10000))

(defn tasks-fn
 [slova
  refs]
 (map (fn [[tacke okvir slovo width]]
       (fn []
        (dosync
         (let [broj-tacaka (count tacke)
               broj-okvira (count okvir)
               broj-p-tacaka (proveri-slovo image x y tacke 0)
               broj-p-okvira (proveri-slovo image x y okvir 255)]
          (alter refs assoc (keyword (str slovo)) )
          )
         
         )
        ;{:slovo t
        ; :procenat (int (* (Math/random) 100))}
         )
       )
      slova))

(defn find-max
 [acc
  [slovo procenat]]
 (if (< (:procenat acc)
        procenat)
  {:slovo (name slovo)
   :procenat procenat}
  acc))

(defn max-in-list
 [results]
 (reduce find-max {:slovo nil
                   :procenat 0} results))

(defn multi-tasking
 [image
  x
  y
  kolekcija]
 (let [refs (ref {})
       pool (Executors/newFixedThreadPool 10)
       tasks (tasks-fn kolekcija
                       refs)]
  (doseq [future (.invokeAll pool tasks)]
   (.get future)
   )
  (.shutdown pool)
  (max-in-list (deref refs))
  ))

;(time (multi-tasking))

#_(let [x (atom 0)
        n-slovo (atom {:slovo nil
                       :procenat 0})]
   (while (< @x
             (+ start 5))
    (let [y (atom 0)]
 ;    (println (str " x: " @x))
     (while (< @y height)
 ;     (println (str " x: " @x " y: " @y))
      (try
       (let [p (.getRGB image @x @y)
             a (bit-and (bit-shift-right p 24) ; transparency
                        127)
             r (bit-and (bit-shift-right p 16) ; red
                        255)
             g (bit-and (bit-shift-right p 8) ; green
                        255)
             b (bit-and (bit-shift-right p 0) ; blue
                        255)
             na-slovo (proveri-slova image @x @y)]
         (when (< (:procenat @n-slovo)
                  (:procenat na-slovo))
          (reset! n-slovo na-slovo))
 ;       (println (str " r: " r " g: " g " b: " b))
        )
       (catch Exception e
 ;       (println e)
        ))
      (swap! y inc))
     )
    (swap! x inc))
   (println @n-slovo))

(let [task1 (fn []
             (Thread/sleep 2000)
             (println "task1"))
      t1 (Thread. task1)
      task3 (fn []
             (Thread/sleep 2000)
             (println "task3"))
      t3 (Thread. task3)
      task2 (fn []
             (.join t1 500)
             (.join t3 500)
             (println "task2"))
      t2 (Thread. task2)]
 (.start t1)
 (.start t2)
 (.start t3)
 )

