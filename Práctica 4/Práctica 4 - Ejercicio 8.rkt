;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 8|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicio 8

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;El Estado del sistema lo representaremos con la estructura Texto que explicaremos
;a continuación.}

(define-struct Texto [s color tam])
;Texto es del tipo (String, String, Number)
;El primer elemento es la cadena a mostrarse, el segundo indican el color de la
;fuente, y el tercero el tamaño de la fuente (en píxeles).


(define ANCHO-ESCENA 800)
(define ALTO-ESCENA 60)

(define ESCENA (empty-scena ANCHO-ESCENA ALTO-ESCENA))

(define TAM-INICIAL 20)
(define COLOR-INICIAL "indigo")

(define ESTADO-INICIAL (make-Texto "" COLOR-INICIAL TAM-INICIAL))

;muestra-texto: Estado -> Image
;Recibe el estado actual del sistema y nos devuelve una imagen con el texto correspondiente al estado.

(define (muestra-texto estado)
  (place-image/align (text (Texto-s estado) (Texto-tam estado) (Texto-color estado))
             10 (/ ALTO-ESCENA 2) "left" "center"
		ESCENA )

;remover-ultimo: String -> String
;Dada una cadena de caracteres, nos devuelve la cadena sin el �ltimo caracter.

(check-expect (remover-ultimo "Hola") "Hol")
(check-expect (remover-ultimo "h") "")

(define (remover-ultimo frase)
  (substring frase 0 (- (string-length frase) 1) )
  )


;teclear: Estado Tecla -> Estado
;Dado el estado actual y la tecla presionada, devuelve un Estado nuevo, con la tecla
;presionada, adicionada al final.

;CORREGIR
(check-expect (teclear " " "\b") "")
(check-expect (teclear "" "\b") "")
(check-expect (teclear "A " "\b") "A")
(check-expect (teclear "hola" "\b") "hol")
(check-expect (teclear "hola" "a") "holaa")
(check-expect (teclear "hola" "A ") "holaA ")

(define (teclear estado tecla)
  (cond [(string=? tecla "\b") (if (= 0 (string-length estado))
                                   estado
                                   (remover-ultimo estado))]
        [else (string-append estado tecla)]
        )
  
  )



(big-bang ESTADO-INICIAL
  [to-draw muestra-texto]
  [on-key teclear])
