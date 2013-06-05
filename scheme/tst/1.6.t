
(add-tests-with-string-output "let"
  [(let ([x 5]) x) => "5\n"]
  [(let ([x (+ 1 2)]) x) => "3\n"]
  [(let ([x (+ 1 2)]) 
     (let ([y (+ 3 4)])
       (+ x y))) 
   => "10\n"]
  [(let ([x (+ 1 2)]) 
     (let ([y (+ 3 4)])
       (- y x)))
   => "4\n"]
  [(let ([x (+ 1 2)]
         [y (+ 3 4)])
     (- y x))
   => "4\n"]
  [(let ([x (let ([y (+ 1 2)]) (* y y))])
     (+ x x))
   => "18\n"]
  [(let ([x (+ 1 2)])
     (let ([x (+ 3 4)])
       x)) 
   => "7\n"]
  [(let ([x (+ 1 2)])
     (let ([x (+ x 4)])
       x)) 
   => "7\n"]
  [(let ([x 1])
     (let ([x (+ x 1)]
	   [y (+ x 1)])
       y)) 
   => "2\n"]
  [(let ([t (let ([t (let ([t (let ([t (+ 1 2)]) t)]) t)]) t)]) t)
   => "3\n"]
  [(let ([x 12])
     (let ([x (+ x x)])
       (let ([x (+ x x)])
         (let ([x (+ x x)])
           (+ x x)))))
   => "192\n"]
)
 
(add-tests-with-string-output "let*"
  [(let* ([x 5]) x) => "5\n"]
  [(let* ([x (+ 1 2)]) x) => "3\n"]
  [(let* ([x (+ 1 2)]) 
     (let* ([y (+ 3 4)])
       (+ x y))) 
   => "10\n"]
  [(let* ([x (+ 1 2)]) 
     (let* ([y (+ 3 4)])
       (- y x)))
   => "4\n"]
  [(let* ([x (+ 1 2)]
         [y (+ 3 4)])
     (- y x))
   => "4\n"]
  [(let* ([x (let* ([y (+ 1 2)]) (* y y))])
     (+ x x))
   => "18\n"]
  [(let* ([x (+ 1 2)])
     (let* ([x (+ 3 4)])
       x)) 
   => "7\n"]
  [(let* ([x (+ 1 2)])
     (let* ([x (+ x 4)])
       x)) 
   => "7\n"]
[(let* ([x 1])
     (let* ([x (+ x 1)]
	   [y (+ x 1)])
       y))
     => "3\n"]
  [(let* ([t (let* ([t (let* ([t (let* ([t (+ 1 2)]) t)]) t)]) t)]) t)
   => "3\n"]
  [(let* ([x 12])
     (let* ([x (+ x x)])
       (let* ([x (+ x x)])
         (let* ([x (+ x x)])
           (+ x x)))))
   => "192\n"]
)
 




