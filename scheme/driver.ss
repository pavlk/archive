;#!r6rs ;; THE DRIVER for SC testing

(define verbose #f)
(define eval-mode #f)

(define (test-one test-id test emitter env)
  (let ([expr (car test)]
	[type (cadr test)]
   	[out (if (not eval-mode)
		 (caddr test)
		 (format "~s\n" (eval (car test) env))
		 )]) 
    (printf "test [~s]:~s  =>  " test-id expr)
    (flush-output-port (current-output-port))
    (case type 
      [(string) (test-with-string-output test-id expr out emitter)]
      [else (error 'test "invalid test type ~s" type)])
    (printf "~s, ok\n" out)))

(define (test-with emitter tests env)
(begin (test-all emitter env)))

(define (run-compile expr emitter)
  (let ([p (open-output-file ".tmp/sc.s" 'replace 'text)])
    (compile-port p)
    (emitter expr)
    (close-output-port p)))

(define (run-compil expr emitter file)
  (let ([p (open-output-file file 'replace 'text)])
    (compile-port p)
    (emitter expr)
    (close-output-port p)))

(define (build)
  (unless (system "gcc -o .tmp/sc tst/runtime-1.9.3.c .tmp/sc.s")
	  (error 'make "could not build target")))

(define (execute)
  (unless (system ".tmp/sc > .tmp/sc.out")
	  (error 'make "produced program exited abnormally")))

(define (get-str file)
  (with-output-to-string
    (lambda ()
      (with-input-from-file file
	(lambda ()
	  (let f ()
	    (let ([c (read-char)])
	      (cond
	       [(eof-object? c) #f]
	       [else (display c) (f)]))))))))

(define (test-with-string-output test-id expr expected-output emitter)
  (run-compile expr emitter)
  (build)
  (execute)
  (unless (string=? expected-output (get-str ".tmp/sc.out"))
	  (error 'test "output mismatch for test ~s, expected ~s, got ~s"
		 test-id expected-output (get-str ".tmp/sc.out"))))

(define (test-all emitter env)
  (let f ([i 1] [ls (reverse tests)])
    (if (null? ls)
	(printf "\n BINGO!!! passed all ~s tests!\n\n" (- i 1))
	(let ([x (car ls)] [ls (cdr ls)])
	  (let* (;[j 1]
		 [test-name (car x)] 
		 [tests (cdr x)]
		 [n (length tests)])
	    (printf "Performing ~a tests ...\n" test-name)
	    (let g ([i i] [tests tests])
	      (cond
	       [(null? tests) (f i ls)]
	       [else
		(test-one i (car tests) emitter env) 
		(g (+ 1 i) (cdr tests))])))))))

(define input-filter 
  (make-parameter (lambda (x) x)
		  (lambda (x)
		    (unless (procedure? x)
			    (error 'input-filter "not a procedure ~s" x))
		    x)))

(define runtime-file 
  (make-parameter
   "runtime.c"
   (lambda (fname)
     (unless (string? fname) (error 'runtime-file "not a string" fname))
     fname)))

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p) 
	     (error 'compile-port "not an output port ~s" p))
     p)))

(define show-compiler-output (make-parameter #f))

(define (emit_n . args) (apply fprintf (compile-port) args))
(define (emit . args) (apply fprintf (compile-port) args) (emit_n "\n"))

(define (lg_n . args) (apply fprintf (current-output-port) args))
(define (lg . args) (if verbose (begin (apply fprintf (current-output-port) args) (lg_n "\n"))))

(define tests '())

(define-syntax add-tests-with-string-output 
  (syntax-rules (=>)
    [(_ test-name [expr => output-string] ...)
     (set! tests
	   (cons 
	    '(test-name [expr string  output-string] ...)
	    tests))]))
