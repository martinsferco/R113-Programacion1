;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_fecha_siguiente) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;______________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA EN AULA - Ejercicio de dia siguiente

;Martín Sferco
;Comisión 1 - Grupo 7
;______________________________________________

;CONSIGNA: DEFINIR UN PROGRAMA QUE DADA UNA FECHA, CALCULE EL DIA SIGUIENTE
;Agregar descripción de funciones y comportamientos

(define (diaSiguiente d m a)
        (if (verificadorDia d m a)
            (cond [(< d (maxMes m a)) (fecha (+ d 1) m a)]
                  [(< m 12) (fecha 1 (+ m 1) a)]
                  [else (fecha 1 1 (+ a 1))]
              )
            "Ingreso una fecha incorrecta"
            )

  )

(define (divisible? x y)
        (= (modulo x y) 0)
  )

(define (bisiesto año)
        (if (divisible? año 4)
            (if (divisible? año 100)
                (divisible? año 400)
                #t)
            #f)
  )

(define (maxMes m a)
        (cond [(or (< m 1)(> m 12)(< a 1)) #f]
              
              [(and (<= m 7) (odd? m)) 31]
              [(and (> m 7) (even? m)) 31]
              [(= m 2) (if (bisiesto a) 29 28)]
              [(and (< m 7) (even? m)) 30]
              [(and (> m 7) (odd? m)) 30]
              )
)

(define (verificadorDia d m a)
        (cond [(boolean? (maxMes m a)) #f]
              [(and (>= d 1)(<= d (maxMes m a))) #t]
              [else #f]
              )
 )

(define (fecha d m a)
        (string-append (number->string d)"/"
                       (number->string m)"/"
                       (number->string a))
  )

(check-expect (diaSiguiente 20 4 2000) "21/4/2000")
(check-expect (diaSiguiente 31 3 2000) "1/4/2000")
(check-expect (diaSiguiente 31 12 2000) "1/1/2001")
(check-expect (diaSiguiente 28 2 2000) "29/2/2000")
(check-expect (diaSiguiente 28 2 2001) "1/3/2001")

(check-expect (diaSiguiente 32 2 2001) "Ingreso una fecha incorrecta")
(check-expect (diaSiguiente 24 13 2001) "Ingreso una fecha incorrecta")
(check-expect (diaSiguiente 32 13 2001) "Ingreso una fecha incorrecta")
(check-expect (diaSiguiente 24 11 0) "Ingreso una fecha incorrecta")

