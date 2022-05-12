;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 11|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicio 11

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;Representaremos una casa con la siguiente estructura.

(define-struct Casa [prop dir sup zona])
;Casa es del tipo (String String Number String)
;El primer campo nos indica el propietario.
;El segundo campo nos indica su direccion (Calle y numero)
;El tercer campo nos indica la superficie en m2.
;El cuarto campo nos indica la zona donde se encuentra (A, B, C o D) 

(define PRECIO-A 20000)
(define PRECIO-B 15000)
(define PRECIO-C 10000)
(define PRECIO-D 5000)

(define SELLADO-MAX 5) ;En %
(define SELLADO-MIN 3) ;En %

;verificador-zona: Casa -> Boolean
;Dada una casa, nos devuelve #t si la zona de la misma, se encuentra
;dentro de los valores por metro cuadrado que tenemos disponibles.
;En caso contrario nos devuelve #f.

(check-expect (verificador-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "A") )#t)
(check-expect (verificador-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "B") )#t)
(check-expect (verificador-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "C") )#t)
(check-expect (verificador-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "D") )#t)
(check-expect (verificador-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "F") )#f)

(define (verificador-zona casa)
  (or (string=? (Casa-zona casa) "A")
      (string=? (Casa-zona casa) "B")
      (string=? (Casa-zona casa) "C")
      (string=? (Casa-zona casa) "D"))
  )
              
;precio-zona: Casa -> Number
;Dada una Casa, nos devuelve el precio por m2, dependiendo de la zona
;en la que se encuentre.

(check-expect (precio-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "A") )PRECIO-A)
(check-expect (precio-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "B") )PRECIO-B)
(check-expect (precio-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "C") )PRECIO-C)
(check-expect (precio-zona (make-Casa "Sferco Martin" "Riobamba 543" 100 "D") )PRECIO-D)

(define (precio-zona casa)
  (cond [(string=? (Casa-zona casa) "A") PRECIO-A]
        [(string=? (Casa-zona casa) "B") PRECIO-B]
        [(string=? (Casa-zona casa) "C") PRECIO-C]
        [(string=? (Casa-zona casa) "D") PRECIO-D])
  )


;porcentaje-recibir: Number -> Number
;Recibe un descuento en porcentaje, y nos devuelve el porcentaje de la casa que tenemos que pagar.
;Hacemos una conversion de porcentaje, sabiendo que 100% es 1, 0% es 0, 50% es 0.5, etc.

(check-expect (porcentaje-recibir SELLADO-MAX) 0.95)
(check-expect (porcentaje-recibir SELLADO-MIN) 0.97)

(define (porcentaje-recibir descuento)
  (- 1 (/ descuento 100))
  )


;monto: Casa -> Number
;Recibe una Casa y nos devuelve el monto que recibe el vendedor por vender la casa.
;Aqui aplicamos los precios que se descontaran por la venta.

(check-expect (monto (make-Casa "Sferco Martin" "Riobamba 444" 120 "C") )1140000)

(define (monto casa)
  (if (>= (* (Casa-sup casa) (precio-zona casa)) 1000000)
      (* (Casa-sup casa) (precio-zona casa) (porcentaje-recibir SELLADO-MAX))
      (* (Casa-sup casa) (precio-zona casa) (porcentaje-recibir SELLADO-MIN))  
  )
)



;construye-frase: String String Number -> String
;Recibe el nombre del propietario, la direccion y el monto a recibir
;y nos devolvera un mensaje unificando toda la informacion.

(check-expect (construye-frase "Sferco Martin" "Riobamba 543" 1223332)
              "Sferco Martin recibira 1223332 pesos por la venta de su propiedad ubicada en la calle Riobamba 543.")
(check-expect (construye-frase "Marto" "Rueda 123" 10)
              "Marto recibira 10 pesos por la venta de su propiedad ubicada en la calle Rueda 123.")

(define (construye-frase prop dir monto)
  (string-append prop " recibira "
                 (number->string monto)
                 " pesos por la venta de su propiedad ubicada en la calle "
                 dir "."))


;venta: Casa -> String
;Toma como entrada un valor de tipo Casa y devuelva un mensaje
;sobre los datos de la venta de dicha propiedad.

(check-expect (venta (make-Casa "Sferco Martin" "Riobamba 543" 100 "Z")) "No se puede calcular el precio de venta por no disponer de los valores del metro cuadrado para la zona solicitada")
(check-expect (venta "Casa") "Tipo de dato incorrecto")
(check-expect (venta (make-Casa "Jose Romero" "Rueda 3456" 120 "C"))
              "Jose Romero recibira 1140000 pesos por la venta de su propiedad ubicada en la calle Rueda 3456.")

(define (venta casa)
  (if (not (Casa? casa)) "Tipo de dato incorrecto"
      (if (not (verificador-zona casa))
          "No se puede calcular el precio de venta por no disponer de los valores del metro cuadrado para la zona solicitada"
          (construye-frase (Casa-prop casa) (Casa-dir casa) (monto casa))
          )
      )
  )