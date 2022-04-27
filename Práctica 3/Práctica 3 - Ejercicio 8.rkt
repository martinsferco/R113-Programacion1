;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 8|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 8

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;El Estado del programa será una imagen,y representará un cielo con una cantidad determinada de
;estrellas.


(define COLOR-ESCENA "lightblue") ;Color fondo escena
(define ANCHO-ESCENA 500)
(define ALTO-ESCENA 500)

(define FONDO-VACIO (rectangle ANCHO-ESCENA ALTO-ESCENA "solid" COLOR-ESCENA)) ;Creación del fondo

(define COLOR-ESTRELLA "yellow")
(define TAMAÑO-MAXIMO-ESTRELLA 50)
(define FACTOR 0.2) ;Determina el factor de reducción que tienen las estrellas totalmente a la izquierda.
(define TAMAÑO-MINIMO-ESTRELLA (* FACTOR TAMAÑO-MAXIMO-ESTRELLA))

;graficador: Estado -> Image
;Recibe el estado actual, y lo representa graficamente.

(define (graficador cielo)
  cielo
  )


;tamaño-estrella: Number -> Number
;Dada la posición horizontal, nos devuelve el tamaño correspondiente de la estrella.
;Considero que totalmente a la izquierda tiene el mínimo valor, y totalmente a la derecha el máximo
;Por lo que nos queda una ecuacion lineal, con coordenada al origen del menor valor,
;y pendiente definida como cociente entre la diferencia de tamaños y el largo total.

(check-expect (tamaño-estrella ANCHO-ESCENA) TAMAÑO-MAXIMO-ESTRELLA)
(check-expect (tamaño-estrella 0) TAMAÑO-MINIMO-ESTRELLA)

(define (tamaño-estrella x)
  (+ (* x (/ (- TAMAÑO-MAXIMO-ESTRELLA TAMAÑO-MINIMO-ESTRELLA) ANCHO-ESCENA))
     TAMAÑO-MINIMO-ESTRELLA)
  )


;estrella: Number -> Image
;Toma la coordenada horizontal, y nos devuelve una estrella correctamente escalada.

(define (estrella x)
  (star (tamaño-estrella x) "solid" COLOR-ESTRELLA)
  )


;verificador-click: Number Number -> Boolean
;Dada las coordenadas del mouse, nos determina si es posible colocar la estrella sin que se nos salga
;de la escena.

(check-expect (verificador-click (/ ANCHO-ESCENA 2) (/ ALTO-ESCENA 2)) #t)
(check-expect (verificador-click 0 ANCHO-ESCENA) #f)
(check-expect (verificador-click 0 ALTO-ESCENA) #f)
(check-expect (verificador-click 0 0) #f)
(check-expect (verificador-click ANCHO-ESCENA ALTO-ESCENA) #f)

(define (verificador-click x y)
  (if (and (>= x (/ (image-width (estrella x)) 2))  
           (<= x (- ANCHO-ESCENA (/ (image-width (estrella x)) 2)))
           (>= y (/ (image-height (estrella x)) 2))
           (<= y (- ALTO-ESCENA (/ (image-height (estrella x)) 2))))
      #t #f
      )
  )

;coloca-estrellas: Estado -> Estado
;Recibe el estado actual, y si se clickea en una zona del cielo, se le agrega una nueva estrella,
;generando un nuevo estado. Las estrellas siempre tienen que estar completamente incluidas en la
;escena.

(define (coloca-estrellas cielo x y acción)
  (cond [(string=? acción "button-down")
         (if (verificador-click x y)
             (place-image (estrella x) x y cielo)
             cielo)]

        [else cielo]
        )
  )


;elimina-estrellas: Estado Tecla -> Estado
;Si se presiona el backspace, se vuelve al estado inicial sin estrellas.
;En cualquier otro caso, se debe mantene el estado como está.

(define (elimina-estrellas cielo tecla)
  (cond [(key=? "\b" tecla) FONDO-VACIO]
        [else cielo]
        )
  )



(define ESTADO-INICIAL FONDO-VACIO)

(big-bang ESTADO-INICIAL
  [to-draw graficador]
  [on-mouse coloca-estrellas]
  [on-key elimina-estrellas]
  )