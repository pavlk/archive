
(add-tests-with-string-output "begin/implicit-begin"
 [(begin 12) => "12\n"]
 [(begin 13 122) => "122\n"]
 [(begin 123 2343 #t) => "#t\n"]
 [(let ([t (begin 12 (cons 1 2))]) (begin t t)) => "(1 . 2)\n"]
 [(let ([t (begin 13 (cons 1 2))])
    (cons 1 t)
    t) => "(1 . 2)\n"]
 [(let ([t (cons 1 2)])
    (if (pair? t) 
        (begin t)
        12)) => "(1 . 2)\n"]
)

(add-tests-with-string-output "set-car! set-cdr!"
  [(let ([x (cons 1 2)])
     (begin (set-cdr! x empty)
     	    x)) => "(1)\n"]
  [(let ([x (cons 1 2)])
     (set-cdr! x empty)
     x) => "(1)\n"]
  [(let ([x (cons 12 13)] [y (cons 14 15)])
     (set-cdr! x y)
     x) => "(12 14 . 15)\n"]
  [(let ([x (cons 12 13)] [y (cons 14 15)])
     (set-cdr! y x)
     y) => "(14 12 . 13)\n"]
  [(let ([x (cons 12 13)] [y (cons 14 15)])
     (set-cdr! y x)
     x) => "(12 . 13)\n"]
  [(let ([x (cons 12 13)] [y (cons 14 15)])
     (set-cdr! x y)
     y) => "(14 . 15)\n"]
  [(let ([x (let ([x (cons 1 2)]) (set-car! x #t) (set-cdr! x #f) x)])
     (cons x x)
     x) => "(#t . #f)\n"]
  [(let ([x (cons 1 2)])
     (set-cdr! x x)
     (set-car! (cdr x) x)
     (cons (eq? x (car x)) (eq? x (cdr x)))) => "(#t . #t)\n"]
 [(let ([x #f])
    (if (pair? x)
        (set-car! x 12)
        #f)
    x) => "#f\n"]
[(let ([x #f])
   (if (pair? #f)
       (set-car! #f 12)
       #f)
   x) => "#f\n"]
)