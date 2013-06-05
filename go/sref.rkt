#lang scheme 
(require srfi/1 srfi/13 srfi/43 mzlib/string rnrs/io/ports-6) (define-namespace-anchor a) 

; UTILITIES
(define-syntax df (syntax-rules () ((df (v e) ...) (begin (define v e) ...)))) 
(define-syntax it (syntax-rules () ((it i n b) (for ((i (iota n))) b))))
(df [:= make-vector] [(:! v i x) (begin (vector-set! v i x) "")] [: vector-ref] [s2n string->number] [app string-append]
    [(:++ v i) (let ((x (+ (vector-ref v i) 1))) (begin (vector-set! v i x) x))] 
    [(:-- v i) (let ((x (- (vector-ref v i) 1))) (begin (vector-set! v i x) x))] 
    [(rm-bts bts str) (bytes->string/utf-8 (regexp-replace* (apply bytes (append '(91) bts '(93))) str ""))]
    [(evals s) (eval (read-from-string s) (namespace-anchor->namespace a))]    
    
; GTP interface 
    [(name) "SRef"] [(version) "2010.01.28"] [(protocol_version) "2"] [K! (:= 1 7)] [(K) (: K! 0)] [(komi v) (:! K! 0 (s2n v))] 
    [cmd '("quit" "protocol_version" "name" "version" "known_command" "list_commands" "d"
                  "komi" "boardsize" "clear_board" "showboard" "genmove" "play" "final_score")] 
    [(gtpln) (string-tokenize (rm-bts (remove* '(0 9 10) (iota 32)) (regexp-replace "#.*" (read-line) "")))] 
    [(known_command str) (if (member str cmd) "true" "false")] 
    [(list_commands) (foldr (lambda (s l) (app (format "~s" s) l)) "" cmd)]
    
; BOARD STATE initialization & handling
    [CAP1 1024] [CAP2 2048] [MASK 1023] [FLAG 64] [ABC "ABCDEFGHIJKLMNOPQRSTUVWXYZ"] [abc "abcdefghijklmnopqrstuvwxyz"]
    [N! (:= 1 9)] [(N) (: N! 0)] [(N1) (+ (N) 1)] [(N2) (+ (N) 2)] [(NN) (* (N) (N))] [(NNN) (+ 1 (* (N1) (N2)))] 
    [BD (:= (NNN) 0)] [VALID (:= (NN) 0)] [(dir) (list 1 -1 (N1) (- 0 (N1)))] [(dia) (list (N) (- 0 (N) (N2) (- 0 (N2))))]
    [C! (:= 1 0)] [(C) (: C! 0)] [HIST (:= (* 4 (NNN)) 0)] [MARKS (:= (* 4 (NNN)) 0)] 
    [(p2s n) (if (= n 0) "." (if (= n 1) "0" "#"))] [(p r c) (+ (* (+ r 1) (N1)) c 1)]       
    
    [(clear_board) (begin (set! BD (:= (NNN) 4)) (set! VALID (:= (NN) 0)) (set! MARKS (:= (* 4 (NNN)) 0)) (set! HIST (:= (* 4 (NNN)) 0))
                         (let ((i 0)) (it r (N) (it c (N) (begin (:! BD (p r c) 0) (:! VALID i (p r c)) (set! i (+ i 1)))))))]     
    
    [(boardsize v) (begin (:! N! 0 (s2n v)) (clear_board) "")]
    [(showboard) (let ((s "")) (begin (it i (N) (begin (set! s (app s "\n")) (it j (N) (set! s (app s (p2s (: BD (p i j)))))))) s))]   
    [(final_score) (let ((s (- (score) (K)))) (if (= s 0) "0" (if (> s 0.0) "B+" "W+")))]   
    [(types m) (let ((t 0)) (begin (it d 3 (set! t (bitwise-ior t (: BD (+ m (list-ref (dir) d)))))) (bitwise-and t 3)))]
    [(score) (let ((s (:= 4 0))) (begin (for [(m VALID)] (let* ([c (: BD m)] [i (if (> c 0) c (types m))]) (:++ s i))) (- (: s 1) (: s 2))))]       
    [(unmark top) (it k top (let ((i (: MARKS k))) (:! BD i (bitwise-xor FLAG (: BD i)))))]
    
    [(libs? t) (let ([top 0] [lib 0] [tcol (: BD t)])
                 (:! MARKS top t) (set! top (+ top 1)) (:! BD t (bitwise-ior FLAG (: BD t)))
                 (do [(j 0 (+ j 1))] [(> j (- top 1))]
                     (do [(i 0 (+ i 1))] [(> i 3)] (let ([x (+ (: MARKS j) (list-ref (dir) i))])
                                                      (if (= 0 (: BD x)) 
                                                          (begin (unmark top) (set! top 0) (set! i 4) (set! lib (+ 1 lib))) ; found liberty, break here!
                                                          (if (= tcol (: BD x)) (begin (:! MARKS top x) (set! top (+ 1 top))
                                                                                       (:! BD x (bitwise-ior FLAG (: BD x)))) 'nil)))))
                 (if (< 0 lib) 0 (begin (unmark top) top)))]    
    
    [(play c m) (if (> 0 (make (s2m m))) "illegal move" "")] [(sb) (printf "~a\n" (showboard))] [(d m) (play "B" m)]  
    [(capture t) (let* ((ncap (libs? t))) (if (< 0 ncap) (it k ncap (:! BD (: MARKS k) 0)) 'nil) ncap)] 
    [(s2m m) (if (or (string-ci= m "pass") (string-ci= m "resign")) 0 (p (- (N) (s2n (string-drop m 1))) (string-index abc (string-ref m 0))))] 
    
    [(make m) (let* ([fstone (- 2 (bitwise-and (C) 1))] [estone (bitwise-xor fstone 3)])
                (cond [(= m 0) (begin (:! HIST (C) 0) (:++ C! 0) 0)] 
                      [(> (: BD m) 0) -3] 
                      [else (let ([cap 0] [x 0]) 
                              (:! BD m fstone) ; place stone
                              (it d 4 (let ([x (+ m (list-ref (dir) d))]) 
                                        (if (= estone (: BD x)) 
                                              (set! cap (+ cap (capture x))) 
                                              'nil)))
                              (cond ((= 0 cap) (if (< 0 (libs? m)) (begin (:! BD m 0) -1) (begin (:! HIST (:++ C! 0) m) cap))) ; if suicide, takeback move, else store it
                                    ((= 1 cap) (let ((last_m (: HIST (- (C) 1)))) (if (and (> 0 (bitwise-and last_m CAP1)) (= 0 (: BD (bitwise-and last_m MASK))))
                                                                                       (begin [:! BD (bitwise-and last_m MASK) estone] [:! BD m 0] -2)
                                                                                       (begin (:! HIST (:++ C! 0) (bitwise-ior m CAP1)) cap))))
                                    (else (begin (:! HIST (:++ C! 0) (bitwise-ior m CAP2)) cap))))]))]
    
; ALGORITHM
    [(genmove) (let ([level 2000]) 0)])

; INIT & LOOP
(clear_board) 
(do [(l (gtpln) (gtpln))] [(equal? l '("quit"))] (if (equal? (known_command (car l)) "false") 
                                                     (printf "? unknown command\n\n")
                                                     (begin (printf "= ~a\n\n" (evals (format "~s" (cons (string->symbol (car l)) (cdr l))))) (sb))))
