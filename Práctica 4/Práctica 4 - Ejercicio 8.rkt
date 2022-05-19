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
;a continuación.

(define-struct Texto [s color tam])
;Texto es del tipo (String, String, Number)
;El primer elemento es la cadena a mostrarse, el segundo indican el color de la
;fuente, y el tercero el tamaño de la fuente (en píxeles).


(define ANCHO-ESCENA 800)
(define ALTO-ESCENA 60)

(define ESCENA (empty-scene ANCHO-ESCENA ALTO-ESCENA))

(define TAM-INICIAL 20)
(define COLOR-INICIAL "black")

(define ESTADO-INICIAL (make-Texto "" COLOR-INICIAL TAM-INICIAL))

;muestra-texto: Estado -> Image
;Recibe el estado actual del sistema y nos devuelve una imagen con el texto correspondiente al estado.

(define (muestra-texto estado)
  (place-image/align (text (Texto-s estado) (Texto-tam estado) (Texto-color estado))
             10 (/ ALTO-ESCENA 2) "left" "center"
		ESCENA )
  )

;remover-ultimo: String -> String
;Dada una cadena de caracteres, nos devuelve la cadena sin el ultimo caracter.

(check-expect (remover-ultimo "Hola") "Hol")
(check-expect (remover-ultimo "h") "")

(define (remover-ultimo frase)
  (substring frase 0 (- (string-length frase) 1) )
  )

;tecla-f?: String -> Boolean
;Dada la tecla presionada, devuelve #t si la tecla es "f1","f2","f3","f4","f5" o "f6".
;En caso contrario devuelve #f.

(check-expect (tecla-f? "f1") #t)
(check-expect (tecla-f? "f6") #t)
(check-expect (tecla-f? "a") #f)
(check-expect (tecla-f? "f7") #f)

(define (tecla-f? tecla)
  (or (key=? tecla "f1")
      (key=? tecla "f2")
      (key=? tecla "f3")
      (key=? tecla "f4")
      (key=? tecla "f5")
      (key=? tecla "f6")
      )
  )

;colorearF: Estado Tecla -> Estado
;Dado el estado actual, y cual de las teclas se presiono, se cambia el color
;del texto, de acuerdo a las siguientes condiciones:
;"f1" - "black"
;"f2" - "red"
;"f3" - "blue"
;"f4" - "lightblue"
;"f5" - "green"
;"f6" - "lightgreen"

(check-expect (colorearF (make-Texto "hola" "green" 20) "f1") (make-Texto "hola" "black" 20))
(check-expect (colorearF (make-Texto "hola" "green" 20) "f6") (make-Texto "hola" "lightgreen" 20))


(define (colorearF estado tecla)
  (cond [(key=? tecla "f1") (make-Texto (Texto-s estado) "black" (Texto-tam estado))]
        [(key=? tecla "f2") (make-Texto (Texto-s estado) "red" (Texto-tam estado))]
        [(key=? tecla "f3") (make-Texto (Texto-s estado) "blue" (Texto-tam estado))]
        [(key=? tecla "f4") (make-Texto (Texto-s estado) "lightblue" (Texto-tam estado))]
        [(key=? tecla "f5") (make-Texto (Texto-s estado) "green" (Texto-tam estado))]
        [(key=? tecla "f6") (make-Texto (Texto-s estado) "lightgreen" (Texto-tam estado))]
        )
  )


;verificador-horizontal: Estado -> Boolean
;Nos determina si un estado entra completamente dentro de la pantalla horizontalmente hablando.

(define (verificador-horizontal estado)
  (< (image-width (text (Texto-s estado) (Texto-tam estado) (Texto-color estado))) ANCHO-ESCENA)
  )

;cambio-tam: Estado Tecla -> Estado
;Dado el estado actual, nos achica o agranda el estado depende si presionamos
;la tecla "down" o "up" respectivamente. La variacion del texto es de a un pixel.
;Tambien se verifica si el texto no se sale de la pantalla.

(check-expect (cambio-tam (make-Texto "hola" "black" 20 ) "up") (make-Texto "hola" "black" 21))
(check-expect (cambio-tam (make-Texto "hola" "black" 1 ) "up") (make-Texto "hola" "black" 2 ))

(define (cambio-tam estado tecla)
  (cond [(key=? tecla "up") (if (< (Texto-tam estado) ALTO-ESCENA)
                                (make-Texto (Texto-s estado) (Texto-color estado) (+ (Texto-tam estado) 1))
                                estado)]
        [else (if (> (Texto-tam estado) 1)
                  (make-Texto (Texto-s estado) (Texto-color estado) (- (Texto-tam estado) 1))
                  estado)]
        )  
  )


;teclear: Estado Tecla -> Estado
;Dado el estado actual y la tecla presionada, devuelve un Estado nuevo, con la tecla
;presionada, adicionada al final.
;Si se presionar el backspace, se elimina un caracter.
;Con las teclas de "f1", "f2", ..., "f6" se cambia el color del texto.
;Con la tecla de "up" y "down" se aumenta y disminuye el tamano de la fuente.
;En este ultimo paso, verificamos que no sea menor a 0, o que se salga de la escena.
 


(check-expect (teclear (make-Texto " " "red" 20) "\b") (make-Texto "" "red" 20))
(check-expect (teclear (make-Texto "" "red" 20) "\b") (make-Texto "" "red" 20))
(check-expect (teclear (make-Texto "A " "red" 20) "\b") (make-Texto "A" "red" 20))
(check-expect (teclear (make-Texto "hola" "red" 20) "\b") (make-Texto "hol" "red" 20))
(check-expect (teclear (make-Texto "hola" "red" 20) "a") (make-Texto "holaa" "red" 20))

(define (teclear estado tecla)
  (cond [(string=? tecla "\b") (if (= 0 (string-length (Texto-s estado)))
                                   estado
                                   (make-Texto (remover-ultimo (Texto-s estado))
                                              (Texto-color estado)
                                              (Texto-tam estado)))]
        
        [(tecla-f? tecla) (colorearF estado tecla)]
        [(key=? tecla "up") (if (verificador-horizontal (cambio-tam estado tecla))
                                                          (cambio-tam estado tecla)
                                                          estado)]
        [(key=? tecla "down") (cambio-tam estado tecla)]
        [else (if (verificador-horizontal (make-Texto (string-append (Texto-s estado) tecla) ;Preguntamos si se sale de pantalla
                                                      (Texto-color estado)
                                                      (Texto-tam estado)))
                  (make-Texto (string-append (Texto-s estado) tecla) ;Si no se sale de pantalla lo mostramos
                              (Texto-color estado)
                              (Texto-tam estado))
                  estado)]
        )
  
  )


(big-bang ESTADO-INICIAL
  [to-draw muestra-texto]
  [on-key teclear])
