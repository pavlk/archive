
(add-tests-with-string-output "vectors"
  [(vector? (make-vector 0)) => "#t\n"]
  [(vector-length (make-vector 12)) => "12\n"]
  [(vector? (cons 1 2)) => "#f\n"]
  [(vector? 1287) => "#f\n"]
  [(vector? empty) => "#f\n"]
  [(vector? #t) => "#f\n"]
  [(vector? #f) => "#f\n"]
  [(pair? (make-vector 12)) => "#f\n"]
  [(null? (make-vector 12)) => "#f\n"]
  [(boolean? (make-vector 12)) => "#f\n"]
  [(make-vector 0) => "#()\n"]
  [(let ([v (make-vector 5)])
     (vector-set! v 0 #t)
     (vector-set! v 1 #f)
     (vector-set! v 2 1)
     (vector-set! v 3 2)
     (vector-set! v 4 empty)
     v) => "#(#t #f 1 2 ())\n"]
  [(let ([v (make-vector 2)])
     (vector-set! v 0 v)
     (vector-set! v 1 v)
     (eq? (vector-ref v 0) (vector-ref v 1))) => "#t\n"]
  [(let ([v (make-vector 1)] [y (cons 1 2)])
     (vector-set! v 0 y)
     (cons y (eq? y (vector-ref v 0)))) => "((1 . 2) . #t)\n"]
    [(let ([v0 (make-vector 2)]
	   [v1 (make-vector 2)])
       (vector-set! v1 0 300)
       (vector-set! v1 1 400)
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (cons v0 v1)) => "(#(100 200) . #(300 400))\n"]
  
[(let ([v0 (make-vector 2)])
     (let ([v1 (make-vector 2)])
       (vector-set! v1 0 300)
       (vector-set! v1 1 400)
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (cons v0 v1))) => "(#(100 200) . #(300 400))\n"]
  [(let ([v0 (make-vector 3)])
     (let ([v1 (make-vector 3)])
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (vector-set! v0 2 150)
       (vector-set! v1 0 300)
       (vector-set! v1 1 400)
       (vector-set! v1 2 350)
       (cons v0 v1))) => "(#(100 200 150) . #(300 400 350))\n"]
  [(let ([n 2])
    (let ([v0 (make-vector n)])
      (let ([v1 (make-vector n)])
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (vector-set! v1 0 300)
       (vector-set! v1 1 400)
       (cons v0 v1)))) => "(#(100 200) . #(300 400))\n"]
  [(let ([n 3])
    (let ([v0 (make-vector n)])
     (let ([v1 (make-vector (vector-length v0))])
       (vector-set! v0 (- (vector-length v0) 3) 100)
       (vector-set! v0 (- (vector-length v1) 2) 200)
       (vector-set! v0 (- (vector-length v0) 1) 150)
       (vector-set! v1 (- (vector-length v1) 3) 300)
       (vector-set! v1 (- (vector-length v0) 2) 400)
       (vector-set! v1 (- (vector-length v1) 1) 350)
       (cons v0 v1)))) => "(#(100 200 150) . #(300 400 350))\n"]
  [(let ([n 1])
     (vector-set! (make-vector n) (sub1 n) (* n n))
     n) => "1\n"]
  [(let ([n 1])
     (let ([v (make-vector 1)])
       (vector-set! v (sub1 n) n)
       (vector-ref v (sub1 n)))) => "1\n"]
 [(let ([v0 (make-vector 1)])
    (vector-set! v0 0 1)
    (let ([v1 (make-vector 1)])
      (vector-set! v1 0 13)
      (vector-set! (if (vector? v0) v0 v1) 
           (sub1 (vector-length (if (vector? v0) v0 v1)))
           (add1 (vector-ref
                      (if (vector? v0) v0 v1)
                      (sub1 (vector-length (if (vector? v0) v0 v1))))))
      (cons v0 v1))) => "(#(2) . #(13))\n"]
)
