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

;CONSIGNA: DEFINIR UN PROGRAMA QUE DADA UNA FECHA, CALCULE EL DIA SIGUIENTE.

;Representaremos a la fecha de entrada como 3 números, los cuales serán día, mes y año.
;El resultado, el cual representa el dís siguiente, lo representaremos como un String de la forma
; "d/m/a".


;divisible?: Number Number -> Boolen
;Dados dos números, devuelve #t si el primero es divisible por el segundo. En caso de que no lo sean
;devuelve #f.

(check-expect (divisible? 10 5) #t)
(check-expect (divisible? 6 4) #f)

(define (divisible? x y)
        (= (modulo x y) 0)
  )

;bisiesto: Number -> Boolean
;Dado un año, devuelve #t si es bisiesto y #f si no lo es.

(check-expect (bisiesto 2000) #t)
(check-expect (bisiesto 1999) #f)

(define (bisiesto año)
        (if (divisible? año 4)
            (if (divisible? año 100)
                (divisible? año 400)
                #t)
            #f)
  )

;maxMes: Number Number -> Number
;Recibe un mes y un año, si estos son incorrecto, devuelve -1. En caso de que sean correctos,
;devolvemos la cantidad de días máximos de ese mes en el correspondiente año.

(check-expect (maxMes 2 2000) 29)
(check-expect (maxMes 2 1999) 28)
(check-expect (maxMes 1 2000) 31)
(check-expect (maxMes 13 2000) -1)

(define (maxMes m a)
        (cond [(or (< m 1)(> m 12)(< a 1)) -1] 
              
              [(and (<= m 7) (odd? m)) 31]
              [(and (> m 7) (even? m)) 31]
              [(= m 2) (if (bisiesto a) 29 28)]
              [(and (< m 7) (even? m)) 30]
              [(and (> m 7) (odd? m)) 30]
              )
)

;verificadorDia: Number Number Number -> False
;Dada una fecha, determina si ésta es correcta o no. Devolviendo un booleano en los casos
;correspondientes.

(check-expect (verificadorDia 1 13 2000) #f)
(check-expect (verificadorDia 1 12 2000) #t)
(check-expect (verificadorDia 29 2 2000) #t)
(check-expect (verificadorDia 28 2 2001) #t)
(check-expect (verificadorDia 32 12 2000) #f)
(check-expect (verificadorDia 1 13 0) #f)

(define (verificadorDia d m a)
        (cond [(< (maxMes m a) 0) #f]
              [(and (>= d 1)(<= d (maxMes m a))) #t]
              [else #f]
              )
 )

;fecha: Number Number Number -> String
;Dados tres números que representan día, mes y año, nos devuelve un string con el formato
;"d/m/a".

(check-expect (fecha 1 12 2000) "1/12/2000")
(check-expect (fecha 3 2 2005) "3/2/2005")

(define (fecha d m a)
        (string-append (number->string d)"/"
                       (number->string m)"/"
                       (number->string a))
  )

;diaSiguiente: Number Number Number -> String
;Dada una fecha, devuelve la fecha del día siguiente.

(check-expect (diaSiguiente 20 4 2000) "21/4/2000")
(check-expect (diaSiguiente 31 3 2000) "1/4/2000")
(check-expect (diaSiguiente 31 12 2000) "1/1/2001")
(check-expect (diaSiguiente 28 2 2000) "29/2/2000")
(check-expect (diaSiguiente 28 2 2001) "1/3/2001")

(check-expect (diaSiguiente 32 2 2001) "Ingreso una fecha incorrecta")
(check-expect (diaSiguiente 24 13 2001) "Ingreso una fecha incorrecta")
(check-expect (diaSiguiente 32 13 2001) "Ingreso una fecha incorrecta")
(check-expect (diaSiguiente 24 11 0) "Ingreso una fecha incorrecta")

(define (diaSiguiente d m a)
        (if (verificadorDia d m a)
            (cond [(< d (maxMes m a)) (fecha (+ d 1) m a)]
                  [(< m 12) (fecha 1 (+ m 1) a)]
                  [else (fecha 1 1 (+ a 1))]
              )
            "Ingreso una fecha incorrecta"
            )

  )


