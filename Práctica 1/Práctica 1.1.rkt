;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 1.1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;------------------------------------------
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 1.1

;Martín Sferco
;Comisión 1 - Grupo 7
;------------------------------------------
;PRIMEROS EJERCICIOS
;------------------------------------------
;1)
;relacion_dimensiones: Image -> String

;relacion_dimensiones toma como argumento una imagen,
;y devuelve Angosta o Ancha dependiendo de la relación
;entre sus dimensiones

(define (relacion_dimensiones image)
        (if (image? image)
            (if (>= (image-height image) (image-width image))
                "La imagen es Angosta" "La imagen es Ancha")
            "No es una imagen"
         )
  )
         
;------------------------------------------
;2)
;relacion_dimensiones_mejora: Image -> String

;relacion_dimensiones_mejora sigue tomando como argumento una image,
; y respecto a la anterior función, se agrega que si los lados son iguales,
; devuelve "Cuadrada"
(define (relacion_dimensiones_mejora image)
        (cond [(> (image-height image) (image-width image)) "La imagen es Angosta"]
              [(< (image-height image) (image-width image)) "La imagen es Ancha"]
              [else "La imagen es Cuadrada"])
)

;------------------------------------------
;3)
;tipo_triangulo: Number Number Number -> String

;tipo_triangulo recibe los 3 ángulos de un triangulo y devuelve
;el tipo de triangulo que es.
(define (tipo_triangulo a b c)
        (cond [(and (= a b) (= b c)) "El triángulo es Equilátero"]
               [(or (= a b)(= b c)(= a c)) "El triángulo es Isósceles"]
               [else "El triángulo es Escaleno"]))

;------------------------------------------
;4)
;tipo_triangulo: Number Number Number -> String

;tipo_triangulo_mejora realiza las mismas funciones que tipo_triangulo,
;solo que agrega que verifica si la suma de los tres argumentos corresponde a la
;suma de los tres ángulos interiores de un triángulo (180 grados)

(define (tipo_triangulo_mejora a b c)
        (cond [(= 180 (+ a b c)) (tipo_triangulo a b c) ]       
              [else "Los tres ángulos que ingreso no corresponden a un triangulo"]
         )
)
 ;------------------------------------------
;5) 

(define PC 60)   ;Precio cuadernos
(define PL 8)    ;Precio lápices
(define DC 0.10) ;Descuento cuadernos
(define DL 0.15) ;Descuento lápices

;descuento: Number Nuber -> Number

;descuento determina cual sera el descuento aplicado al cliente, dependiendo
;de cuales fueron las cantidades que compraron de lápices y cuadernos
(define (descuento c l)
        (+ (if (>= l 5) (* PL l (- 1 DL)) (* PL l) )
           (if (>= l 4) (* PC c (- 1 DC)) (* PC c) )
         )
 )

;Se podría mejorar si definimos una función aparte que determine los descuentos por
;separado de cada unos de los objetos de la librería


;descuento_objeto: Number Number Number Number-> Number

;descuento_objeto toma como arguentos el número de objetos, cual es el límite para que se empieze a aplicar
;el descuento, su precio y su descuento,y nos devuelve cual es el precio a pagar.
(define (descuento_objeto n lim P D) 
        (if (>= n lim) (* P n (- 1 D)) (* P n))
  )

;descuento_mejora: Number Number -> Number
(define (descuento_mejora c l)
        (+ (descuento_objeto c 4 PC DC)(descuento_objeto l 5 PL DL))
  )

;------------------------------------------
;6)
;nuevo_descuento: Number Number -> Number

;nuevo_descuento, además de aplicar los descuentos anteriores, si el cliente lleva
;más de 10 unidades se le hace un descuento total del 18%. Solo se aplicará el descuento que le
;al cliente el mayor beneficio
(define (nuevo_descuento c l)
        (cond [(>= (+ c l) 10) (min (descuento_mejora c l)
                                     (* (- 1 0.18) (+ (* PC c)(* PL l))))]

              [else (descuento_mejora c l)]
              )
  )

;------------------------------------------
;7)
;comprueba: Number Number Number -> Boolean

;comprueba toma 3 números y comprueba si forman una terna pitagórica, tomando como mayor número
;al argumento z
(define (comprueba x y z)
        (= (+ (sqr x)(sqr y)) (sqr z))
  )

;pitagorica?: Number Number Number -> Boolean

;pitagorica?, toma tres números y devuelve si ellos forman una terna pitagórica,
(define (pitagorica? x y z)
        (cond [(and (> z x)(> z y)) (comprueba x y z)]
              [(and (> y x)(> y z)) (comprueba x z y)]
              [else (comprueba z y x)]
              )
)

;------------------------------------------
;8)
;juntador: Number Number Number -> String

;juntador nos aydua a concatenar 3 números usando comas.
(define (juntador a b c)
        (string-append (number->string a) ", " (number->string b) " y " (number->string c))
 )

;pitagorica?_mejora: Number Number Number -> String

;pitagorica?_mejora, devuelve un String si los tres números forman una terna pitagórica.
(define (pitagorica?_mejora x y z)
        (if (pitagorica? x y z)
           (string-append "Los números " (juntador x y z) " forman una terna pitagórica")
           (string-append "Los números " (juntador x y z) " no forman una terna pitagórica")
           )
)

;------------------------------------------
;9)
;collatz: Number-> Number

;collatz, recibe un número natural n y devuelve la mitad si n es par
;o devuelve 3n+1 si n es impar
(define (collatz n)
        (if (and (integer? n) (> n 0))
            (if (= 0 (modulo n 2))
                (/ n 2)
                (+ (* 3 n) 1)
             )
            "El número que ingreso no es Natural"
         )
  )





;__________________________________________
;EJERCICIOS DE BANDERAS
;Las banderas tienen 90 pixeles de ancho por 60 de alto, y se ubican sobre un empty-scene
;de las mismas dimensiones
;__________________________________________
(define ESCALE 10) ;Escala de las banderas

(define HB (* 60 ESCALE)) ;Altura bandera
(define WB (* 90 ESCALE)) ;Largo bandera

(define HHR (/ HB 3)) ;Altura de rectángulo horizontal
(define WVR (/ WB 3)) ;Ancho de rectángulo vertical

(define CUADH (/ HB 6)) ;Cuadrícula de organización a lo alto
(define CUADW (/ WB 6)) ;Cuadrícula de organización a lo ancho
;------------------------------------------
;Bandera de Perú

;PERU: Image
(define PERU (place-image (rectangle WVR HB "solid" "red")
                          CUADW (/ HB 2)
                          (place-image (rectangle WVR HB "solid" "white")
                                       (* CUADW 3) (/ HB 2)
                          (place-image (rectangle WVR HB "solid" "red")
                                       (* CUADW 5) (/ HB 2)
                                       (empty-scene WB HB))))
)

;------------------------------------------
;Bandera de Italia

;ITALIA: Image
(define ITALIA (place-image (rectangle WVR HB "solid" "lime")
                          CUADW (/ HB 2)
                          (place-image (rectangle WVR HB "solid" "white")
                                       (* CUADW 3) (/ HB 2)
                          (place-image (rectangle WVR HB "solid" "red")
                                       (* CUADW 5) (/ HB 2)
                                       (empty-scene  WB HB)))))

;------------------------------------------
;bandera_vertical: String String String -> Image

;bandera_vertical es una función que toma tres colores (en orden de izquierda a derecha)
;y devuelve la bandera con tres franjas vertciales correspondiente a esos colores
(define (bandera_vertical color1 color2 color3)
        (place-image (rectangle WVR HB "solid" color1)
                          CUADW (/ HB 2)
                          (place-image (rectangle WVR HB "solid" color2)
                                       (* CUADW 3) (/ HB 2)
                          (place-image (rectangle WVR HB "solid" color3)
                                       (* CUADW 5) (/ HB 2)
                                       (empty-scene  WB HB)))))

;------------------------------------------
;Bandera de Alemania

;ALEMANIA: Image
(define ALEMANIA (place-image (rectangle WB HHR "solid" "black")
                          (/ WB 2) CUADH
                          (place-image (rectangle WB HHR "solid" "red")
                                       (/ WB 2) (* CUADH 3)
                          (place-image (rectangle WB HHR "solid" "yellow")
                                       (/ WB 2) (* CUADH 5)
                                       (empty-scene  WB HB))))
)


;------------------------------------------
;Bandera de Holanda

;HOLANDA: Image
(define HOLANDA (place-image (rectangle WB HHR "solid" "red")
                          (/ WB 2) CUADH
                          (place-image (rectangle WB HHR "solid" "white")
                                       (/ WB 2) (* CUADH 3)
                          (place-image (rectangle WB HHR "solid" "blue")
                                       (/ WB 2) (* CUADH 5)
                                       (empty-scene  WB HB))))
)

;------------------------------------------
;bandera_horizontal: String String String -> Image

;bandera_horizontal es una función que toma tres colores (en orden de arriba a abajo)
;y devuelve la bandera con tres franjas horizontales correspondiente a esos colores
(define (bandera_horizontal color1 color2 color3)
        (place-image (rectangle WB HHR "solid" color1)
                          (/ WB 2) CUADH
                          (place-image (rectangle WB HHR "solid" color2)
                                       (/ WB 2) (* CUADH 3)
                          (place-image (rectangle WB HHR "solid" color3)
                                       (/ WB 2) (* CUADH 5)
                                       (empty-scene  WB HB)))))

;------------------------------------------
;bandera_generica: String String String String -> Image

;bandera_generica es una función que toma un sentido de orientación (vertical/horizontal),
;y tres valores de color (tomados de izquierda a derecha o de arriba a abajo, según corresponda)
(define (bandera_generica orientation color1 color2 color3)
        (cond [(string=? "horizontal" orientation) (bandera_horizontal color1 color2 color3)]
              [(string=? "vertical" orientation) (bandera_vertical color1 color2 color3)]
              [else "No es correcta la orientación de bandera que ingreso"])
  )


;------------------------------------------
;Bandera de Francia, Sudán, Argentina y Camerún

;FRANCIA: Image
(define FRANCIA (bandera_generica "vertical" "blue" "white" "Medium Red"))

;SUDAN: Image
(define SUDAN (place-image (rotate 30 (triangle HB "solid" "green" ))
                           (* (/ HB 4) (sqrt 3)) (/ HB 2)
                           (bandera_generica "horizontal" "red" "white" "black")))

;ARGENTINA: Image
(define ARGENTINA (place-image (radial-star 20 (* HHR 1/2 0.6) (* HHR 1/2 0.9) "solid" "yellow")
                           (/ WB 2) (/ HB 2)
                           (bandera_generica "horizontal" "Deep Sky Blue" "white" "Deep Sky Blue")))

;CAMERUN: Image
(define CAMERUN (place-image (star (* WVR 1/2 0.7) "solid" "yellow" )
                           (/ WB 2) (/ HB 2)
                           (bandera_generica "vertical" "green" "red" "yellow")))


;BRASIL: Image
(define BRASIL (place-image (circle (* WVR 1/2 0.9) "solid" "blue")
                            (/ WB 2) (/ HB 2)
                            (place-image (rhombus (* WVR 1.5) 120 "solid" "yellow" )
                                         (/ WB 2) (/ HB 2)
                                         (bandera_generica "vertical" "Lawn Green" "Lawn Green" "Lawn Green"))))

             