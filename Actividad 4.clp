;; Definir hechos
(deftemplate cliente
   (slot id)
   (slot nombre)
   (slot correo)
   (slot cel))

(deftemplate producto
   (slot marca)
   (slot modelo)
   (slot memoria)
   (slot precio)
   (slot existencia))

(deftemplate tarjeta
   (slot nombre)
   (slot banco)
   (slot meses-sin-intereses))

(deftemplate compra
   (slot id-cliente)
   (slot productos)
   (slot tarjeta)
   (slot total))

;; Definir reglas
(defrule oferta-iphone-banamex
   (compra (productos ?productos&:(member (marca "Apple") ?productos)
            &:(member (modelo "iPhone 14") ?productos))
           (tarjeta (banco "Banamex")))
   =>
   (printout t "Oferta: 24 meses sin intereses." crlf))

(defrule oferta-samsung-liverpool
   (compra (productos ?productos&:(member (marca "Samsung") ?productos)
            &:(member (modelo "X") ?productos))
           (tarjeta (banco "Liverpool VISA")))
   =>
   (printout t "Oferta: 12 meses sin intereses." crlf))

(defrule descuento-applewatch-iphone
   (compra (productos ?productos&:(member (marca "Apple") ?productos)
            &:(member (modelo "Watch") ?productos)
            &:(member (modelo "iPhone 14") ?productos)))
   =>
   (bind ?total (get-compra-total ?productos))
   (bind ?descuento (* 0.1 (/ ?total 1000)))
   (printout t "Descuento: " ?descuento " pesos." crlf))

(defrule descuento-funda-mica
   (compra (productos ?productos&:(member (categoria "Smartphone") ?productos)))
   =>
   (bind ?total (get-compra-total ?productos))
   (bind ?descuento (* 0.15 (get-smartphone-precio ?productos)))
   (printout t "Descuento: " ?descuento " pesos." crlf))

(defrule sugerencia-existencia
   (compra (productos ?productos))
   (not (producto (existencia 0)))
   =>
   (printout t "Sugerencia: Revisar el catálogo para ver otros productos disponibles." crlf))

;; Funciones auxiliares
(deffunction get-compra-total (?productos)
   (bind ?total 0)
   (foreach ?producto ?productos
      (bind ?total (+ ?total (slot-value ?producto precio))))
   ?total)

(deffunction get-smartphone-precio (?productos)
   (foreach ?producto ?productos
      (if (eq (slot-value ?producto categoria) "Smartphone")
         then (return (slot-value ?producto precio)))))