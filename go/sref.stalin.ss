; simple GTP based Go bot

; Notes:
; DOES NOT KNOW: eval, string-tokenize, regexp-replace, foldr, current-command-line-arguments
; KNOWS: iota, (read-line (input-port))

; UTILITIES
(define := make-vector) 
;(define (:! v i x) (vector-set! v i x))
(define :! vector-set!)
(define : vector-ref)
(define s2n string->number) 
(define app string-append)
(define (:++ v i) (let ((x (vector-ref v i))) (begin (vector-set! v i (+ x 1)) x)))
(define (:-- v i) (let ((x (vector-ref v i))) (begin (vector-set! v i (- x 1)) x)))

; random number generator    
(define seed 1)
;(define !> inexact->exact)
(define (congru_ m a) (begin (set! seed (modulo (+ a (* seed a)) m)) seed))
(define (congru) (congru_ (expt 2 20) 3412421))
(define (random-range n) (>> (* n (bitwise-and (congru) #xFFFF)) 16))
;(define random-range random-integer)

; GTP INTERFACE
(define (name) "SRef") 
(define (version) "2010.02.28") 
(define (protocol_version) "2") 
(define K! (:= 1 7)) (define (K) (: K! 0)) (define (komi v) (:! K! 0 (s2n v)))
(define cmd '("quit" "protocol_version" "name" "version" "known_command" "list_commands" "d"
                  "komi" "boardsize" "clear_board" "showboard" "genmove" "play" "final_score"))
;(define (gtpln)(string-tokenize (rm-bts (remove* '(0 9 10) (iota 32)) (regexp-replace "#.*" (read-line) ""))))
(define (known_command str) (if (member str cmd) "true" "false"))
;(deifne (list_commands) (foldr (lambda (s l) (app (format "~s" s) l)) "" cmd))
    
; BOARD STATE initialization & handling
(define CAP1 1024) (define CAP2 2048) (define MASK 1023) (define FLAG 64) (define ABC "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (define abc "abcdefghijklmnopqrstuvwxyz")
(define N 9) 
(define N1 (+ N 1)) (define N2 (+ N 2)) (define NN (* N N)) (define NNN (+ 1 (* N1 N2))) 
(define BD (:= NNN 0)) (define VALID (:= NN 0)) (define dir (list 1 -1 N1 (- 0 N1))) (define dia (list N (- 0 N N2 (- 0 N2))))
(define C 0) 
(define (C++) (begin (set! C (+ C 1)) (- C 1)))
(define (C--) (begin (set! C (- C 1)) (+ C 1)))

(define HIST (:= (* 4 NNN) 0)) (define MARKS (:= (* 4 NNN) 0)) 
(define (p2s n) (if (= n 0) "." (if (= n 1) "0" "#"))) (define (p r c) (+ (* (+ r 1) N1) c 1))

;(define (boardsize v) (begin (:! N! 0 (s2n v)) (clear_board) ""))
;(define (showboard) (let ((s "")) (begin (for ((i (iota (N)))) (begin (set! s (app s "\n")) (for ((j (iota (N)))) (set! s (app s (p2s (: BD (p i j)))))))) s)))
;(define (sb2) (let ((s "")) (begin (for ((i (iota (N)))) (begin (set! s (app s "\n")) (for ((j (iota (N)))) (set! s (app s (number->string (: BD (p i j)) 10)))))) s)))
(define (final_score) (let ((s (- (score) (K)))) (if (= s 0) "0" (if (> s 0.0) "B+" "W+"))))   
(define (types m) (let ((t 0)) (begin 
				 (do ((d 0 (+ d 1))) ((= d 2))
				      (set! t (bitwise-or t (: BD (+ m (list-ref dir d)))))) (bitwise-and t 3))))
(define (score) (let ((s (:= 4 0))) (begin (do ((k 0 (+ k 1))) ((= k NN)) (let* ((m (: VALID k))
										 (c (: BD m)) (i (if (> c 0) c (types m)))) (:++ s i))) (- (: s 1) (: s 2)))))  
(define (unmark top) (do ((k 0 (+ k 1))) ((= k top)) (let ((i (: MARKS k))) (:! BD i (bitwise-xor FLAG (: BD i))))))

(define (init) (begin (set! C 0) (set! BD (:= NNN 4)) (set! VALID (:= NN 0)) (set! MARKS (:= (* 4 NNN) 0)) (set! HIST (:= (* 4 NNN) 0))
		      (let ((i 0)) (do ((r 0 (+ r 1))) ((= r N)) (do ((c 0 (+ c 1))) ((= c N)) (begin (:! BD (p r c) 0) (:! VALID i (p r c)) (set! i (+ i 1))))))))     
(define (clear_board) (begin (set! C 0) 
			     (do ((i 0 (+ i 1))) ((= i C)) (:! HIST i 0))
                             (do ((r 0 (+ r 1))) ((= r N)) (do ((c 0 (+ c 1))) ((= c N)) (:! BD (p r c) 0)))))
 
; STONE OPERATIONS
;(define (s2m m) (if (or (string-ci= m "pass") (string-ci= m "resign")) 0 (p (- N (s2n (string-drop m 1))) (string-index abc (string-ref m 0))))) 
;(define (m2s m) (if (= 0 m) "pass" (format "~a~s" (string-ref abc (- (modulo m N1) 1)) (- N (- (floor (/ m N1)) 1)))))
;(define (play c m) (if (> 0 (make (s2m m))) "illegal move" "")) 
;(define (sb) (printf "~a\n" (showboard))) (define (d m) (play "B" m)) (define (s) (printf "~a\n" (sb2)))
(define (capture t) (let* ((ncap (libs? t))) (if (< 0 ncap) (do ((k 0 (+ k 1)))((= k ncap)) (:! BD (: MARKS k) 0)) 'nil) ncap)) 
(define (libs? t) (let ((top 0) (lib 0) (tcol (: BD t)))
                    (:! MARKS top t) (set! top (+ top 1)) (:! BD t (bitwise-or FLAG (: BD t)))
                    (do ((j 0 (+ j 1))) ((> j (- top 1)))
                      (let ((tt (: MARKS j)))
                      (do ((i 0 (+ i 1))) ((> i 3)) (let ((x (+ tt (list-ref dir i)))) 
                                                      (if (= 0 (: BD x)) 
                                                          (begin (unmark top) (set! j top) (set! i 4) (set! lib (+ 1 lib))) ; found liberty, break here!
                                                          (if (= tcol (: BD x)) (begin            
										  (:! MARKS top x) (set! top (+ 1 top))
										  (:! BD x (bitwise-or FLAG (: BD x)))) 'nil))))))
                    (if (< 0 lib) 0 (begin (unmark top) top))))
    
(define (make m) (let* ((fstone (- 2 (bitwise-and C 1))) (estone (bitwise-xor fstone 3)))
                   (cond ((= m 0) (begin (:! HIST C 0) (C++) 0)) 
                         ((> (: BD m) 0) -3) 
                         (else (let ((cap 0) (x 0)) 
                                 (:! BD m fstone) ; place stone
                                 (do ((d 0 (+ d 1))) ((= d 4)) (let ((x (+ m (list-ref dir d)))) 
                                           (if (= estone (: BD x)) 
                                               (set! cap (+ cap (capture x))) 
                                               'nil)))
                                 (cond ((= 0 cap) (if (< 0 (libs? m)) (begin (:! BD m 0) -1) (begin (:! HIST (C++) m) cap))) ; if suicide, takeback move, else store it
                                       ((= 1 cap) (let ((last_m (: HIST (- C 1)))) (if (and (not (= 0 (bitwise-and last_m CAP1))) (= 0 (: BD (bitwise-and last_m MASK))))
                                                                                         (begin (:! BD (bitwise-and last_m MASK) estone) (:! BD m 0) -2)
                                                                                         (begin (:! HIST (C++) (bitwise-or m CAP1)) cap))))
                                       (else (begin (:! HIST (C++) (bitwise-or m CAP2)) cap))))))))
    
; THE ALGORITHM 
(define level 1000000)
(define c 0)
(define (genmove nil) (begin (mc-genmove level) C))
(define (r-mv) (make (: VALID (random-range NN))))
(define (mc-genmove level) (do ((i 0 (+ i 1))) ((= i level)) (begin (clear_board) (do ((r (r-mv) (r-mv))) ((= r -1)) #f))))

; INITIALIZE & LOOP
(init)(clear_board) 
(genmove "B")
(display seed)