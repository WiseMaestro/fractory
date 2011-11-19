(ns fractory.core)


(require 'fractory.colormaps)

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
  (let [ adjustcount  (* threshold count)] (cond (>= adjustcount  255) 0
                                                (> (+ (* real real) (* imag imag)) 4)
                                                    (min 255 adjustcount)
                                        ;return number of iterations til escape
                                                :else (let [sq (isq (list real imag))
                                                            rc (first c)
                                                            ic (second c)
                                                            r (+ rc (first sq))
                                                            im (+ ic (second sq))]
                                                        (recur r im c (+ 1 count) threshold)
                                                        )
                                                ))
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
          ]
  (let [newcolor (mandelgetcolor scalex
                                 scaley
                                 (list scalex scaley) 0 threshold)]
    (list xypair (take 3 (repeat newcolor))) ; 3 means rgb
    )))



(defn drawfractal 
    ([boundsx boundsy scalefactor complex threshold]
      (let [frm (javax.swing.JFrame.)
            pan (.getContentPane frm)                                        ;GRAPHICS ABOVE
            scal scalefactor                                        ;How large is the graphic
            lowx (first boundsx)
            highx (second boundsx)
            lowy (first boundsy)
            highy (second boundsy)
            ]


    (doto frm (.setSize (java.awt.Dimension. scal scal)) (.show))
    (let [xypair (for [xs (range 0 scal) ys (range 0 scal)] (list xs ys))
       noproc (.. Runtime getRuntime availableProcessors)
            xysets (partition-all (int (+ 1 (/  (* scal scal)
                                                noproc)))
                                  
                                  xypair)               
       coorcolors  (pmap
                    #(doall  (map scaleandcolorj (repeat lowx) (repeat lowy) (repeat scal) %1
                                  (repeat highx) (repeat highy) (repeat complex) (repeat threshold)))
                           xysets)

;                  xysets (partition-all (int (+ 1 (/  (* scal scal) 4))) xypair)               
;                  coorcolors (pmap scaleandcolorrj (repeat lowx) (repeat lowy) (repeat scal) xypair
 ;                  (repeat highx) (repeat highy) (repeat complex) (repeat threshold))
            grp (.getGraphics pan)
            ] 
      
#_       (doseq [pixels coorcolors]
         (let [xcoor (first (first pixels)) ycoor (second (first pixels))
               red (first (second pixels)) green (second (second pixels))
               blue (nth (second pixels) 2)]
                      (.setColor grp (java.awt.Color. 0 (int (* 0.3 green)) blue))
                      (try (.drawLine grp xcoor ycoor xcoor ycoor))
                      ))
       (doseq [pixelnest coorcolors]
         (doseq [pixels pixelnest] (let [xcoor (first (first pixels)) ycoor (second (first pixels))
               red (first (second pixels)) green (second (second pixels))
               blue (nth (second pixels) 2)]
                      (.setColor grp (java.awt.Color. 0 (int (* 0.3 green)) blue))
                      (try (.drawLine grp xcoor ycoor xcoor ycoor))
                      ))
        )




)))
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

#_        (let [grp (.getGraphics pan)]
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

 (let [xypair (for [xs (range 0 scal) ys (range 0 scal)] (list xs ys))
      noproc (.. Runtime getRuntime availableProcessors)
            xysets (partition-all (int (+ 1 (/  (* scal scal)
                                                noproc)))
                                  
                                  xypair)               
       coorcolors  (pmap
                    #(doall  (map scaleandcolor (repeat lowx) (repeat lowy) (repeat scal) %1
                                  (repeat highx) (repeat highy) (repeat threshold)))
                           xysets)
            
            grp (.getGraphics pan)]
        
      
       (doseq [pixelnest coorcolors]
         (doseq [pixels pixelnest] (let [xcoor (first (first pixels)) ycoor (second (first pixels))
               red (first (second pixels)) green (second (second pixels))
               blue (nth (second pixels) 2)]
                     #_ (.setColor grp (java.awt.Color. 0 (int (* 0.3 green)) blue))
                     (.setColor grp (nth colormaps/colormap (mod blue (count colormaps/colormap))))
                     
                     
                     (try (.drawLine grp xcoor ycoor xcoor ycoor)))))))))

(nth colormaps/colormap 3)


;(mandelgetcolor -0.7 0.3 '(-1.6 0.3) 0)
(time (drawfractal  '(-2 2) '(-1.5 2.5) 500 '(0.5 -0.5) 12))
;Aly wants this picture below
(drawfractal  '(-2 2) '(-1.5 2.5) 500 '(0 0.9) 10)
(time (drawfractal (zoom 6 '(-1.15 0.85)) (zoom 6 '(-1.2 0.8)) 1000 '(0 0.9) 7))
(time (drawfractal  (zoom 4 '(-1.35 0))  (zoom 4 '(-1.8 -0.3)) 600 '(1 0) 10))

(time (drawfractal  '(-2.5 2.5) '(-2.5 2.5) 1000 '(0.25 0.75) 10))
(time (drawfractal  '(-2 2) '(-1.8 2.2) 1000 '(0.1 -0.7) 5))
(time (drawfractal  '(-2 2) '(-1.8 2.2) 1000 '(0.5 -0.7) 16))

(time (drawfractal  '(-2.1 1.5) '(-1.7 1.7) 400 1))
(time (drawfractal (zoom 450 '(-0.055 0.055)) (zoom 450 '(0.72992 0.84245)) 300 '(-1 0) 1))


                                        ;seahorsevalley
                                        ;deeper and deeper

 (drawfractal '(-0.9 -0.6) '(0.1 0.3) 1000 1)
 (drawfractal (zoom 50 '(-0.8 -0.65)) (zoom 50 '(0.15 0.35)) 1000 1)
 (drawfractal (zoom 1250 '(-0.8 -0.65)) (zoom 1250 '(0.15 0.35)) 1000 1)
 (drawfractal (zoom 1250 (zoom 1250 '(-0.8 -0.65))) (zoom 1250 (zoom 1250 '(0.15 0.35))) 500 1)
 (* 1250 1250)













