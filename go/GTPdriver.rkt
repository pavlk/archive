#lang racket 

; a simple GTP driver for a (reference) computer-GO bot
; motivation: it allows you to write the bot as a simple commandline implementing the function [m] -> m, where m is a move number
; now you can write simpler go reference bots in any programming language without dealing with the GTP interface

; by: Pavol Vlcek, 2010-08-12 

(require srfi/1 srfi/13 srfi/43 mzlib/string rnrs/io/ports-6 rnrs/arithmetic/bitwise-6) 

(define (version) "2010.08.12")
(define argv (current-command-line-arguments)) 
(define ALG (if (>= (vector-length argv) 1) (vector-ref argv 0) "101"))
(define L   (if (>= (vector-length argv) 2) (string->number (vector-ref argv 1)) 100))
(define N 9) ; boardsize
(define K 7) ; komi

(define-namespace-anchor a) 
(define-syntax it (syntax-rules () ((it i sz b) (for ((i (iota sz))) b))))
(define := make-vector) (define app string-append)
(define :! vector-set!) (define (oI x y) (bitwise-ior x y)) 
(define (::! V1 V2) (do ((i 0 (+ i 1))) ((= i (vector-length V2)) V2) (:! V2 i (vector-ref V1 i )))) 
(define (:++ s i) (:! s i (+ 1 (vector-ref s i))))
(define (rm-bts bts str) (bytes->string/utf-8 (regexp-replace* (apply bytes (append '(91) bts '(93))) str "")))
(define (evals s) (eval (read-from-string s) (namespace-anchor->namespace a)))
(define (gtpln) (string-tokenize (rm-bts (remove* '(0 9 10) (iota 32)) (regexp-replace "#.*" (read-line) "")))) 
    
(define ABC "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define N1 (+ N 1)) 
(define N2 (+ N 2)) 
(define NN (* N N)) 
(define NNN (+ 1 (* N1 N2))) 
(define vdir (vector 1 -1 N1 (- 0 N1))) 
(define vdia (vector N (- 0 N) N2 (- 0 N2)))
(define v4 (:= 4))
(define (p2s n) (if (= n 0) "." (if (= n 1) "O" "#"))) 
(define (p r c) (+ (* (+ r 1) N1) c 1))       

(define cmd '("quit" "protocol_version" "name" "version" "known_command" "list_commands" "g" "s"
       "komi" "boardsize" "clear_board" "showboard" "genmove" "play" "final_score")) 
(define (protocol_version) "2") 
(define (name) "SRef") 
(define (known_command str) (if (member str cmd) "true" "false")) 
(define (list_commands) (foldr (lambda (s l) (app (format "~s" s) l)) "" cmd))
(define (komi v) (begin (set! K (string->number v)) ""))
(define (boardsize v) (set! N (string->number v)) (clear_board) "")
(define (clear_board) (set! NMS 0) (it r N (it c N (:! BRD (p r c) 0))) "")
(define (showboard) (let ((s "")) (it i N (begin (set! s (app s "\n")) (it j N (set! s (app  s " " (p2s (vector-ref BRD (p i j)))))))) s))
(define (s2m m) (if (or (string-ci= m "pass") (string-ci= m "resign")) 0 
		    (p (- N (string->number (string-drop m 1))) (string-index ABC (string-ref m 0))))) 
(define (m2s m) (if (= 0 m) "pass" (format "~a~s" (string-ref ABC (- (modulo m N1) 1)) (- N (- (floor (/ m N1)) 1)))))
(define (play c m) (let* ((mv (s2m m)) (res (make BRD NMS mv 0 #f))) (if (> 0 res) "illegal move" (begin (save-m HIS res mv) ""))))
(define (final_score) (let ((s (score BRD))) (if (= s 0) "0" (app (if (> s 0.0) "B+"  "W+") (number->string (abs s))))))  
(define (ref-playouts) (number->string L))
;(define (ref-score) (number->string (+ 0.5 (* 0.5 (/ (G-score G) L)))))
;(define (ref-nodes) (number->string (G-nodes G)))
(define (g) (genmove 'c)) (define (s) (showboard))

; INITIALIZE & LOOP
(define BRD (:= NNN 0)) (define HIS (:= (* 4 NNN) 0)) 
(define NMS 0) (define (NMS++) (begin (set! NMS (+ NMS 1)) (if (> NMS 0) (- NMS 1) 0)))
(define P (:= NN 0)) 

(define (save-m H res m)
  (when (>= res 0) (if (= res 0)
	    (:! H (NMS++) m)
	    (begin (if (= res 1) 
		       (:! H (NMS++) m) 
 		       (:! H (NMS++) m))))))

(define (init) (let ((i 0)) (set! BRD (:= NNN 4)) (set! HIS (:= (* 4 NNN) 0)) 
		    (clear_board)
		    (it r N (it c N (begin (:! BRD (p r c) 0) (:! P i (p r c)) (set! i (+ i 1)))))))     

(define (read-from-process process args l)
  (let-values (((s s-out s-in s-err) (apply subprocess #f #f #f process args l)))
	      (read s-out)))

(define (genmove nil) 
  (let ((m (read-from-process ALG 
			      (number->string L) 
			      (map number->string (vector->list (::! HIS (:= NMS)))))))
    (if (equal? "" (play 'c  (m2s m))) 
	(m2s m) 
	"pass")))

(define MARKS (:= (+ 2 NN) 0))

(define (mark B i m) 
 (:! MARKS i m)
  (:! B m (oI 64 (vector-ref B m)))) ;optim: netreba, staci MARKS?

(define (unmark B top) 
  (:for-n (lambda (x) (:! B x (xI 64 (vector-ref B x)))) 
	  MARKS top))

(define (capture B t) 
  (let ((n (libs? B t))) 
    (:for-n (lambda (x) (:! B x 0)) MARKS n)
    n))

(define (libs? B t) 
  (let ((top 1) (tcol (vector-ref B t)))
    (mark B 0 t) 
    (do ((j 0 (+ j 1))) ((>= j top) 
			 (unmark B top) 
			 (if (= j top) top 0))
      (for-nbrs (vector-ref MARKS j) (lambda (x) (if (= 0 (vector-ref B x)) 
					    (set! j 9999) 
					    (when (= tcol (vector-ref B x)) 
						  (mark B top x) 
						  (set! top (+ 1 top)))))))))

(define (make B ms m last_m ko) 
  (letrec ((cap 0))
    (cond ((= m 0) 0) ; pass
	  ((> (vector-ref B m) 0) -3) ; occupied
	  (else (begin (:! B m (fstone ms)) ; place stone
		       (let ((x 0)) (do ((d 0 (+ d 1))) ((= d 4)) 
				      (set! x (+ m (vector-ref vdir d)))
				      (when (= (estone ms) (vector-ref B x)) 
					    (set! cap (+ cap (capture B x))))))
		       (for-nbrs m (lambda (y) (when (= (estone ms) (vector-ref B y)) (set! cap (+ cap (capture B y))))))
		       (cond ((= 0 cap) (if (< 0 (libs? B m))      ; suicide? 
					    (begin (:! B m 0) 
						   -1)             ; takeback m
 					    cap))                  ; store m
			     ((= 1 cap) (if (and ko (= 0 (vector-ref B last_m)))
					    (begin (:! B last_m (estone ms)) 
						   (:! B m 0) 
						   -2) ; ko
					    cap))
			     (else cap)))))))

(define (& x y) (bitwise-and x y)) (define (xI x y) (bitwise-xor x y))
(define :foldl vector-fold) ;f init V) (do ((i 0 (+ i 1))) ((= i (:l V)) init) (set! init (f init (vector-ref V i)))))
(define (:for-each f V) (do ((i 0 (+ i 1))) ((= i (vector-length V))) (f (vector-ref V i))))
(define (:for-v-i f V) (do ((i 0 (+ i 1))) ((= i (vector-length V))) (f (vector-ref V i) i)))
(define (:for-n f V n) (do ((i 0 (+ i 1))) ((= i n)) (f (vector-ref V i))))
(define (fstone c) (- 2 (& c 1))) 
(define (estone c) (xI 3 (- 2 (& c 1))))
(define (for-nbrs m f) (:for-each (lambda (x) (f (+ m x))) vdir))  ; optim: predpocitat (neighb-inds m)

(define (neighbours B m) 
  (:for-v-i (lambda (d i) (:! v4 i (vector-ref B (+ m d)))) vdir)
  v4)

(define (owner-color B m) 
  (if (= (vector-ref B m) 0) 
      (& 3 (:foldl oI 0 (neighbours B m))) 
      (vector-ref B m)))

(define (score B) 
  (let ((s (:= 3 0))) 
    (:for-each (lambda (m) (:++ s (owner-color B m))) P) 
    (- (vector-ref s 2) (vector-ref s 1) K)))

(init) 
(do ((l (gtpln) (gtpln))) ((equal? l '("quit")) (printf "= \n\n")) 
  (if (equal? (known_command (car l)) "false") 
      (printf "? unknown command\n\n")
      (begin (printf "= ~a\n\n" (evals (format "~s" (cons (string->symbol (car l)) (cdr l))))))))
