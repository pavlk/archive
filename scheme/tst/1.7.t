
(add-tests-with-string-output "procedures"
  [(letrec () 12) => "12\n"];;1
  [(letrec () (let ([x 5]) (+ x x))) => "10\n"];;2
  [(letrec ([f (lambda () 5)]) 7) => "7\n"];;3
  [(letrec ([f (lambda () 5)]) (let ([x 12]) x)) => "12\n"];;4
  [(letrec ([f (lambda () 5)]) (f)) => "5\n"];;5
  [(letrec ([f (lambda () 5)]) (let ([x (f)]) x)) => "5\n"];;6
  [(letrec ([f (lambda () 5)]) (+ (f) 6)) => "11\n"];;7
  [(letrec ([f (lambda () 5)]) (- 20 (f))) => "15\n"];;8
  [(letrec ([f (lambda () 5)]) (+ (f) (f))) => "10\n"];;9
  [(letrec ([f (lambda () (+ 5 7))]
            [g (lambda () 13)]) 
    (+ (f) (g))) => "25\n"];;10
  [(letrec ([f (lambda (x) (+ x 12))]) (f 13)) => "25\n"];;11
  [(letrec ([f (lambda (x) (+ x 12))]) (f (f 10))) => "34\n"];;12
  [(letrec ([f (lambda (x) (+ x 12))]) (f (f (f 0)))) => "36\n"];;13
  [(letrec ([f (lambda (x y) (+ x y))]
            [g (lambda (x) (+ x 12))])
    (f 16 (f (g 0) (+ 1 (g 0))))) => "41\n"];;14
  [(letrec ([f (lambda (x) (g x x))]
            [g (lambda (x y) (+ x y))])
     (f 12)) => "24\n"];;15
  [(letrec ([f (lambda (x) 
                 (if (zero? x)
                     1
                     (* x (f (sub1 x)))))])
      (f 5)) => "120\n"];;16
 [(letrec ([e (lambda (x) (if (>= 0 x) x (o (sub1 x))))]
            [o (lambda (x) (if (>= 0 x) (add1 x) (e (sub1 x))))])
     (o 11)) => "0\n"];;17
  [(letrec ([e (lambda (x) (if (zero? x) x (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) (add1 x) (e (sub1 x))))])
     (e 11)) => "1\n"];;18
 [(letrec ([e (lambda (x) (if (zero? x) #\a (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) #\b (e (sub1 x))))])
     (o 11)) => "#\\a\n"];;19
 [(letrec ([e (lambda (x) (if (zero? x) #\a (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) #\b (e (sub1 x))))])
     (e 11)) => "#\\b\n"];;21
 [(letrec ([e (lambda (x) (if (zero? x) #t (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) #f (e (sub1 x))))])
     (o 11)) => "#t\n"];;20 
 [(letrec ([e (lambda (x) (if (zero? x) #t (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) #f (e (sub1 x))))])
     (e 11)) => "#f\n"];;22
)
