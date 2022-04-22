;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 1.2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 1.2

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________
;Ejercicio 4)-

;clasificador_imagen: Image -> String

;clasificador_imagen toma una imagen y devuelve un String dependiendo de la relación que existe entre sus lados
(define (image_class image)
        (cond [(> (image-width image) (* 2 (image-height image))) "Muy gorda"]
              [(> (image-height image) (* 2 (image-width image))) "Muy flaca"]
              [(> (image-width image) (image-height image)) "Gorda"]
              [(> (image-height image) (image-width image)) "Flaca"]
              [else "Cuadrada"])
  )

;Probamos los 5 casos que se nos proporcionan
(check-expect (image_class (rectangle 10 10 "solid" "blue")) "Cuadrada")
(check-expect (image_class (rectangle 30 10 "solid" "blue")) "Muy gorda")
(check-expect (image_class (rectangle 10 30 "solid" "blue")) "Muy flaca")
(check-expect (image_class (rectangle 15 10 "solid" "blue")) "Gorda")
(check-expect (image_class (rectangle 10 15 "solid" "blue")) "Flaca")

;_________________________________________
;Ejercicio 5)-

;clasificar: Number -> String

;clasificar toma un número que representa la temperatura y nos devuelve una frase acorde
;al rango de temperaturas en el que nos encontramos
(define (clasificar t)
        (cond [(<= t 0) "No olvidar bufanda (NOB)"]
              [(<= t 15) "Frio (F)"]
              [(<= t 25) "Agradable (A)"]
              [else "Realmente caluroso"]
              )
  )

;__________________________________________
;Ejercicio 6)-

;signo: Number -> Number

;signo toma un número como argumento y devuelve 0 si es 0, 1 si es positivo y -1 si es negativo
(define (signo x)
        (cond [(< x 0) -1]
              [(= x 0) 0]
              [(> x 0) 1]
              )
  )



;signo_mejora1 ahora nos permite clasificar strings
(define (signo_mejora1 x)
        (cond [(number? x) (signo x)]
              [(string? x) (signo (string->number x))]
              )
  )

;signo_mejora2 ahora nos permite clasificar Booleanos sabiendo que #t equivale a 1 y #f a 0
(define (signo_mejora2 x)
        (cond [(number? x) (signo x)]
              [(string? x) (signo (string->number x))]
              [(boolean? x) (if (boolean=? x #t) 1 0)]
          )
  )

;__________________________________________
;Ejercicio 7)-

;signo_mejora3 ahora tambien nos permite clasificar imágenes, diciendo que si la imagen es Gorda o Muy gorda
;es equivalente a 1. si es Flaca o Muy flaca equivale a -1, y si es Cuadrada equivale a 0.
(define (signo_mejora3 x)
        (cond [(number? x) (signo x)]
              [(string? x) (signo (string->number x))]
              [(boolean? x) (if (boolean=? x #t) 1 0)]
              [(image? x) (cond [(or (string=? "Muy gorda" (image_class x))
                                     (string=? "Gorda" (image_class x)))
                                 1]
                                [(or (string=? "Muy flaca" (image_class x))
                                     (string=? "Flaca" (image_class x)))
                                 -1]
                                [else 0]
                                )]
  )      
)

;__________________________________________
;Ejercicio 8)-

;signo_mejora4 nos devuelve un string que dice "Clase no soportada por la función" en caso de que
;se le pase como argumento un valor que no entra en ninguna de las clasificaciones
(define (signo_mejora4 x)
        (cond [(number? x) (signo x)]
              [(string? x) (signo (string->number x))]
              [(boolean? x) (if (boolean=? x #t) 1 0)]
              [(image? x) (cond [(or (string=? "Muy gorda" (image_class x))
                                     (string=? "Gorda" (image_class x)))
                                 1]
                                [(or (string=? "Muy flaca" (image_class x))
                                     (string=? "Flaca" (image_class x)))
                                 -1]
                                [else 0]
                                )]
              
              [else "Clase no soportada por la función"]
  )      
)

;__________________________________________
;Ejercicio 9)-

;signo_mejora5 nos permite ver si un string esta compuesto completamente por números. En caso contrario
;nos devuelve un string que dice "La cadena no se puede convertir a un número"
(define (signo_mejora5 x)
        (cond [(number? x) (signo x)]
              [(string? x) (if (string-numeric? x)
                               (signo (string->number x))
                               "La cadena no se puede convertir en número")]
              [(boolean? x) (if (boolean=? x #t) 1 0)]
              [(image? x) (cond [(or (string=? "Muy gorda" (image_class x))
                                     (string=? "Gorda" (image_class x)))
                                 1]
                                [(or (string=? "Muy flaca" (image_class x))
                                     (string=? "Flaca" (image_class x)))
                                 -1]
                                [else 0]
                                )]
              
              [else "Clase no soportada por la función"]
  )      
)

;Probamos todas las posible salidas de signo_mejora5
(check-expect (signo_mejora5 4) 1)
(check-expect (signo_mejora5 -4) -1)
(check-expect (signo_mejora5 0) 0)
(check-expect (signo_mejora5 #t) 1)
(check-expect (signo_mejora5 #f) 0)
(check-expect (signo_mejora5 "hola") "La cadena no se puede convertir en número")
(check-expect (signo_mejora5 "123") 1)
(check-expect (signo_mejora5 "0") 0)

(check-expect (signo_mejora5 (rectangle 10 10 "solid" "blue")) 0)
(check-expect (signo_mejora5 (rectangle 30 10 "solid" "blue")) 1)
(check-expect (signo_mejora5 (rectangle 10 30 "solid" "blue")) -1)
(check-expect (signo_mejora5 (rectangle 15 10 "solid" "blue")) 1)
(check-expect (signo_mejora5 (rectangle 10 15 "solid" "blue")) -1)


