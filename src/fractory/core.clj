(ns core)

(defn isq [complex]
  (let [real (first complex)
        imag (second complex)]
    (list (- (* real real) (* imag imag))
         (* 2 real imag))
    )
  )

(defn zoom ( [zx zy xbounds ybounds]
               (let [lowx (first xbounds)
                     highx (second xbounds)
                     diffx (- highx lowx)
                     center (/ (+ lowx highx) 2)
                     newhighx (float (+ center (/ diffx (* 2 zx))))
                     newlowx (float (- center (/ diffx (* 2 zx))))
                     lowy (first ybounds)
                     highy (second ybounds)
                     diffy (- highy lowy)
                     center (/ (+ lowy highy) 2)
                     newhighy (float (+ center (/ diffy (* 2 zy))))
                     newlowy (float (- center (/ diffy (* 2 zy))))
                     ]
                 (list (list newlowx newhighx) (list newlowy newhighy))
                 ))
  ([zx xbounds]
     (let [lowx (first xbounds)                                                        
           highx (second xbounds)                                                      
           diffx (- highx lowx)                                                        
           center (/ (+ lowx highx) 2)                                                 
           newhighx (float (+ center (/ diffx (* 2 zx))))                           
           newlowx (float (- center (/ diffx (* 2 zx))))  ]
       (list newlowx newhighx)
     )))

(defn scaleit [scale num newscale]
  (double  (* newscale (/ num scale))))


(defn mandelgetcolor [real imag c count threshold]
  (cond (= count 254) count
        (> (+ (* real real) (* imag imag)) 4)
                (min 254 (* threshold count)) ;return number of iterations til escape
        :else (let [sq (isq (list real imag))
                    rc (first c)
                    ic (second c)
                    r (+ rc (first sq))
                    im (+ ic (second sq))]
                (recur r im c (+ 1 count) threshold)
                )
   )
  )

(defn scaleandcolorj [lowx lowy scal xypair highx highy complex threshold]
  (let [newcolor (mandelgetcolor (+ lowx (scaleit scal (first  xypair) (- highx lowx)))
                                 (+ lowy (scaleit scal (second  xypair) (- highy lowy)))
                                 complex 0 threshold)]
    (list xypair (take 3 (repeat newcolor))) ; 3 means rgb
    )
  )

(defn scaleandcolor [lowx lowy scal xypair highx highy threshold]
  (let [scalex (+ lowx (scaleit scal (first xypair) (- highx lowx)))
        scaley (+ lowy (scaleit scal (second  xypair) (- highy lowy)))
        newcolor (mandelgetcolor scalex
                                 scaley
                                 (list scalex scaley)
                                 0 threshold)]
    (list xypair (take 3 (repeat newcolor))) ; 3 means rgb
    )
  )


(defn drawfractal ([boundsx boundsy scalefactor complex threshold]

  (let [frm (javax.swing.JFrame.)
        pan (.getContentPane frm)
        ;GRAPHICS ABOVE
        scal scalefactor
        ;How large is the graphic
        lowx (first boundsx)
        highx (second boundsx)

        lowy (first boundsy)
        highy (second boundsy)
          ]
      (doto frm (.setSize (java.awt.Dimension. scal scal)) (.show))
            (let [xypair (for [xs (range 0 scal) ys (range 0 scal)] (list xs ys))
            coorcolors (map scaleandcolorj (repeat lowx) (repeat lowy) (repeat scal) xypair
                                    (repeat highx) (repeat highy) (repeat complex) (repeat threshold))
            grp (.getGraphics pan)
            ] 
      
       (doseq [pixels coorcolors]
         (let [xcoor (first (first pixels)) ycoor (second (first pixels))
               red (first (second pixels)) green (second (second pixels))
               blue (nth (second pixels) 2)]
                      (.setColor grp (java.awt.Color. 0 (int (* 0.3 green)) blue))
                      (try (.drawLine grp xcoor ycoor xcoor ycoor))
                      )))

      ))
  ([boundsx boundsy scalefactor threshold]
      (let [frm (javax.swing.JFrame.)
            pan (.getContentPane frm)                                        ;GRAPHICS ABOVE
            scal scalefactor                                        ;How large is the graphic
            lowx (first boundsx)
            highx (second boundsx)
            lowy (first boundsy)
            highy (second boundsy)
            ]
   
        (doto frm (.setSize (java.awt.Dimension. scal scal)) (.show))

        (let [grp (.getGraphics pan)]
          (doseq [xs (range 0 scal) ys (range 0 scal)]
            (let [realscale (+ lowx (scaleit scal xs (- highx lowx)))
                  imagscale (+ lowy (scaleit scal ys (- highy lowy)))
                  newcolor (mandelgetcolor
                            realscale
                            imagscale
                            (list realscale imagscale) 0 threshold)]
              (.setColor grp (java.awt.Color.
                              0 (int (* 0.3 newcolor)) newcolor)
                         )
               
              (try (.drawLine grp xs ys xs ys)))))




#_      (let [xypair (for [xs (range 0 scal) ys (range 0 scal)] (list xs ys))
            coorcolors (map scaleandcolor (repeat lowx) (repeat lowy) (repeat scal) xypair
                                    (repeat highx) (repeat highy) (repeat threshold))
            grp (.getGraphics pan)
            ] 
      
       (doseq [pixels coorcolors]
         (let [xcoor (first (first pixels)) ycoor (second (first pixels))
               red (first (second pixels)) green (second (second pixels))
               blue (nth (second pixels) 2)]
                      (.setColor grp (java.awt.Color. 0 (int (* 0.3 green)) blue))
                      (try (.drawLine grp xcoor ycoor xcoor ycoor))
                      ))))
      )
  )














(isq (2 -2))




;(mandelgetcolor -0.7 0.3 '(-1.6 0.3) 0)
(time (drawfractal  '(-2 2) '(-1.5 2.5) 500 '(0.5 -0.5) 12))
;Aly wants this picture below
(drawfractal  '(-2 2) '(-1.5 2.5) 500 '(0 0.9) 10)
(time (drawfractal (zoom 6 '(-1.15 0.85)) (zoom 6 '(-1.2 0.8)) 1000 '(0 0.9) 7))
(time (drawfractal  (zoom 4 '(-1.35 0))  (zoom 4 '(-1.8 -0.3)) 600 '(1 0) 10))
(time (drawfractal  '(-2.5 2.5) '(-2.5 2.5) 1000 '(3 -3) 20))


(time (drawfractal  '(-2.1 1.5) '(-1.7 1.7) 900 1))
(time (drawfractal (zoom 450 '(-0.055 0.055)) (zoom 450 '(0.72992 0.84245)) 300 '(-1 0) 1))
                                    ;seahorsevalley
(time (drawfractal '(-0.9 -0.6) '(0.1 0.3) 800 1))















