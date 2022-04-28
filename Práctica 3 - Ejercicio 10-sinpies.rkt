;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 10-sinpies|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 10

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________
;El Estado será un String
;El primer caracter de la cadena será la dirección hacia
;donde se camina, utilizando:
;d = derecha
;i = izquierda
;EJEMPLOS: "d150" o "i30"
;Los restantes números de la cadena representarán la coordenada
;horizontal del centro del pie que se encuentre apoyado en un
;momento dado.

;Definimos la escena y sus dimensiones.
(define ANCHO-ESCENA 1000)
(define ALTO-ESCENA 200)
(define ESCENA (empty-scene ANCHO-ESCENA ALTO-ESCENA))


;Por ahora los representamos con rectángulos.


(define PIEIZQ (square 50 "solid" "black"))
(define PIEDER (square 50 "solid" "black"))



(define DISTANCIA-PASOS 55)

;Defino dos constantes que me permiten alinear los pies
;a lo alto de la escena.

(define ALINEA-PIEDER (* 2/3 ALTO-ESCENA))
(define ALINEA-PIEIZQ (* 1/3 ALTO-ESCENA))
;__________________________________________


;rotador-huella: Image -> Image
;Dada una huella, nos devuelve la imagen rotada 180 grados.

(define (rotador-huella huella)
  (rotate 180 huella)
  )

;posicion-estado: Estado -> Number
;Dado el estado, nos devuelve la posición asociada al estado

(check-expect (posicion-estado "d150") 150)
(check-expect (posicion-estado "i1") 1)

(define (posicion-estado estado)
  (string->number (substring estado 1 (string-length estado)))
  )

;paridad-estado: Estado -> Boolean
;Dado el estado actual, nos devuelve #t si la posición horizontal
;es par y #f si es impar.

(check-expect (paridad-estado "d150") #t)
(check-expect (paridad-estado "i151") #f)

(define (paridad-estado estado)
  (even? (posicion-estado estado))
  )

;derecha?: Estado -> Boolean
;Dado el estado, nos devuelve #t si está yendo a la derecha y #f si está yendo a la izquierda.

(check-expect (derecha? "d150") #t)
(check-expect (derecha? "i150") #f)

(define (derecha? estado)
  (string=? "d" (string-ith estado 0)))


;forma-estado: String Number -> String
;Dada la dirección de caminata y la posición horizontal de la huella actual,
;nos devuelve un string juntando ambos valores, siguiendo el orden que especificamos
;al principio del programa.

(check-expect (forma-estado "d" 150) "d150")
(check-expect (forma-estado "i" 150) "i150")
(check-expect (forma-estado "d" 30) "d30")
(check-expect (forma-estado "i" 30) "i30")

(define (forma-estado direccion posicion)
  (string-append direccion (number->string posicion))
  )
  

;representador-estado: Estado -> Image
;Recibe el estado actual y nos devuelve una imagen de la representación
;actual del estado.

(define (representador-estado estado)
  (cond [(derecha? estado) (if (paridad-estado estado)
                        (place-image PIEIZQ (posicion-estado estado) ALINEA-PIEIZQ ESCENA)
                        (place-image PIEDER (posicion-estado estado) ALINEA-PIEDER ESCENA))]
                        
        [else (if (paridad-estado estado)
                        (place-image (rotador-huella PIEIZQ) (posicion-estado estado) ALINEA-PIEDER ESCENA)
                        (place-image (rotador-huella PIEDER) (posicion-estado estado) ALINEA-PIEIZQ ESCENA))]
        )
     )


;verificador-huella-entera: Estado -> Boolean
;Dada la direccion de desplazamiento y la posición horizontal, determina si la siguiente huella
;entrará completa o no. En caso de que entre, devuelve #t y #f en caso contrario.

(define (verificador-huella-entera estado)
  (cond [(derecha? estado) (< (+ (posicion-estado estado)
                                  DISTANCIA-PASOS
                                  (/ (image-width PIEDER) 2))
                              ANCHO-ESCENA)]
        [else (> (- (posicion-estado estado)
                    DISTANCIA-PASOS
                    (/ (image-width PIEDER) 2))
                 0)]
        )
  )

;mover-huellas: Estado -> Estado
;Dado el estado actual, devuelve el siguiente estado, de acuerdo a la distacia
;de pasos y la dirección de la caminata.

(define (mover-huellas estado)
  (if (verificador-huella-entera estado)
      (cond [(derecha? estado) (forma-estado "d" (+ (posicion-estado estado) DISTANCIA-PASOS ))]
            [else (forma-estado "i" (- (posicion-estado estado) DISTANCIA-PASOS ))]
            )
      (cond [(derecha? estado)  (forma-estado "i" (- ANCHO-ESCENA (/ (image-width PIEDER) 2)))]
            [else ESTADO-INICIAL])
      )
  )


;reinicia-caminata: Estado String -> Estado
;Dado el estado actual y la tecla presionada, si se presiona el espacio, se vuelve al estado inicial.
;En caso contrario, no hacemos nada.

(check-expect (reinicia-caminata "d150" " ") ESTADO-INICIAL)
(check-expect (reinicia-caminata "d150" "k") "d150")

(define (reinicia-caminata estado tecla)
  (cond [(key=? tecla " ") ESTADO-INICIAL]
        [else estado]
    )
  )

(define ESTADO-INICIAL (forma-estado "d" (/ (image-width PIEDER) 2)))

(big-bang ESTADO-INICIAL
  [to-draw representador-estado]
  [on-key reinicia-caminata]
  [on-tick mover-huellas 0.5]
  )




