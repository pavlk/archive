(add-tests-with-string-output "deeply nested procedures"
  [(letrec ([sum (lambda (n ac)
                   (if (zero? n)
                        ac
                        (sum (sub1 n) (+ n ac))))])
    (sum 7 0)) => "28\n"];;24
  [(letrec ([e (lambda (x) (if (zero? x) #t (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) #f (e (sub1 x))))])
     (e 3)) => "#f\n"];;25
  [(letrec ([e (lambda (x) (if (zero? x) #t (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) #f (e (sub1 x))))])
     (e 6)) => "#t\n"];;26
  [(letrec ([sum (lambda (n ac)
                   (if (zero? n)
                        ac
                        (sum (sub1 n) (+ n ac))))])
    (sum 10000 0)) => "50005000\n"];;26
  [(letrec ([e (lambda (x) (if (zero? x) #t (o (sub1 x))))]
            [o (lambda (x) (if (zero? x) #f (e (sub1 x))))])
     (e 5000000)) => "#t\n"];;27
 )