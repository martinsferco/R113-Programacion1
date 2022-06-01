;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Dosbolas_eljuego) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define ANCHO 500)
(define ALTO 500)
(define ESCENA (empty-scene ANCHO ALTO))

(define R1 30)
(define R2 50)

(define DELTA 10)


(define C1 (circle R1 "solid" "red"))
(define C2 (circle R2 "solid" "blue"))

(define-struct Coor [x1 y1 x2 y2])
;Es del estilo (Number Number Number Number)

(define INICIAL (make-Coor R1 R1 (- ANCHO R2) (- ANCHO R2)))
(define FINAL (make-Coor -1 -1 -1 -1))



(define (render coor)
  (if (not (terminar coor))
       (place-image C1 (Coor-x1 coor) (Coor-y1 coor)
               (place-image C2 (Coor-x2 coor) (Coor-y2 coor) ESCENA))
       (overlay (text "FIN DEL JUEGO" 20 "black") ESCENA)
       )
  )


(define (mover-personaje coor k n)
  (cond [(or (key=? k "w")(key=? k "up")) (if (= n 1) (make-Coor (Coor-x1 coor) (- (Coor-y1 coor) DELTA) (Coor-x2 coor) (Coor-y2 coor))
                                                      (make-Coor (Coor-x1 coor) (Coor-y1 coor)  (Coor-x2 coor) (- (Coor-y2 coor) DELTA)))]
        [(or (key=? k "s")(key=? k "down")) (if (= n 1) (make-Coor (Coor-x1 coor) (+ (Coor-y1 coor) DELTA) (Coor-x2 coor) (Coor-y2 coor))
                                                      (make-Coor (Coor-x1 coor) (Coor-y1 coor)  (Coor-x2 coor) (+ (Coor-y2 coor) DELTA)))]
        [(or (key=? k "a")(key=? k "left")) (if (= n 1) (make-Coor (- (Coor-x1 coor) DELTA) (Coor-y1 coor) (Coor-x2 coor) (Coor-y2 coor))
                                                      (make-Coor (Coor-x1 coor) (Coor-y1 coor)  (- (Coor-x2 coor) DELTA) (Coor-y2 coor)))]
        [(or (key=? k "d")(key=? k "right")) (if (= n 1) (make-Coor (+ (Coor-x1 coor) DELTA) (Coor-y1 coor) (Coor-x2 coor) (Coor-y2 coor))
                                                      (make-Coor (Coor-x1 coor) (Coor-y1 coor)  (+ (Coor-x2 coor) DELTA) (Coor-y2 coor)))]
        )
  )

(define (distancia coor)
  (sqrt (+ (sqr (- (Coor-x1 coor) (Coor-x2 coor)))(sqr (-(Coor-y1 coor) (Coor-y2 coor)))))
  )


(define (controlador-teclado coor k)
  (cond [(or (key=? k "w")(key=? k "s")(key=? k "a")(key=? k "d")) (mover-personaje coor k 1)]
        [(or (key=? k "up")(key=? k "down")(key=? k "left")(key=? k "right")) (mover-personaje coor k 2)]
        [else coor]
        )

  )


(define (terminar coor)
  (< (distancia coor) (+ R1 R2))
  )



(big-bang INICIAL
  [to-draw render]
  [on-key controlador-teclado]
  [stop-when terminar render]
  )