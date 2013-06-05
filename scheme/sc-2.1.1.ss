#!r6rs  ;; THE SCHEME COMPILER, run with 'mzscheme filename.scm'
;; implemented by following this tutorial: http://lambda-the-ultimate.org/node/1752

(import (rnrs eval-6) (except (rnrs) close-output-port open-output-file flush-output-port) (mzlib compat) (scheme system) (scheme base-modif))
(include "driver.ss") 

;; CONSTANTS & HELPERS
(define sc_version "2.1.1")
(define fx_shift 2) 
(define fx_mask #x03)			;; 11b
(define fx_neg_mask 2147483648)         ;; 1(31x0)b mask for negative numbers
(define fx_tag #x00)	                ;; 00b
(define fx_to_mask 4294967292)          ;; (30x1)00b mask to null last 2 bits in a 32bit number
(define char_shift 8) 
(define char_mask #xFF)   
(define char_tag #x0F)			;; 15, 1111b
(define char_dist1 256)
(define heapobj_mask #x07)		;; 111b
(define pair_tag #x01)			;; 001b
(define closure_tag #x02)		;; 010b
(define symbol_tag #x03)	     	;; 011b
(define vector_tag #x05)                ;; 101b
(define string_tag #x06)	      	;; 110b
(define bool_f #x2F)			;; 47,  0010 1111b
(define bool_t #x6F)			;; 111, 0110 1111b
(define bool_bit #x06) 
(define bool_mask #xBF)			;; 1011 1111b
(define bool_tag bool_f) 
(define null_list #x3F)			;; 63,  0011 1111b 
(define wordsize 4)			;; bytes 

(define (next-si si) (- si wordsize))
(define (emit-si-save si) (emit " movl %eax, ~s(%esp)" si))
(define (emit-si-load si) (emit " movl ~s(%esp), %eax" si))
(define fixnum-bits (- (* wordsize 8) fx_shift)) 
(define fxlower (- (expt 2 (- fixnum-bits 1)))) 
(define fxupper (- (expt 2 (- fixnum-bits 1)) 1)) 
(define (myfixnum? x) (and (integer? x) (exact? x) (<= fxlower x fxupper))) 
(define (immediate? x) (or (myfixnum? x) (boolean? x) (char? x) (myempty? x))) 
(define (primitive? x) (and (symbol? x) (getprop x '*is-prim*))) 
(define (primcall? expr) (and (pair? expr) (primitive? (car expr)))) 
(define (myempty? expr) (and (list? expr)) (symbol? expr) (eq? 'empty expr))
(define (form? expr type) (and (list? expr)) (symbol? (car expr)) (eq? type (car expr)))
(define (fcall? expr env) (and (list? expr) (symbol? (car expr)) (lookup (car expr) env) (not (number? (lookup (car expr) env)))))
(define (expr? expr) (or (immediate? expr) (variable? expr) (primcall? expr) (not (form? expr 'letrec))))
(define (variable? expr) (symbol? expr)) 
(define (emit-immediate x) (emit " movl $~s, %eax" (immediate-rep x)))
(define (primitive-emitter x) (or (getprop x '*emitter*) (error "Parameter is not an emitter!"))) 

(define (immediate-rep x) 
  (cond [(myfixnum? x) (bitwise-arithmetic-shift x fx_shift)] 
	[(boolean? x) (cond [x bool_t]
			    [else bool_f])]
	[(char? x) (bitwise-ior (bitwise-arithmetic-shift (char->integer x) 8) char_tag)] 
	[(myempty? x) null_list])) 

(define-syntax define-primitive 
  (syntax-rules () [(_ (prim-name si arg* ...) b b* ...) 
		    (begin 
		      (putprop 'prim-name '*is-prim* #t) 
		      (putprop 'prim-name '*arg-count* 
			       (length '(arg* ...))) 
		      (putprop 'prim-name '*emitter* 
			       (lambda (si arg* ...) () b b* ...)))])) 

(define (emit-primcall si env expr) 
  (let ([prim (car expr)] [args (cdr expr)]) 
    (if (= 1 (length args)) (emit-expr si env (car args)) (emit-arguments si args env)) 
    (apply (primitive-emitter prim) si env args)))

(define (emit-cmp-msk si env arg mask tag)
  (emit-expr si env arg) 
  (emit " and $~s, %al" mask) 
  (emit " cmp $~s, %al" tag) 
  (emit-cmp-res)) 

(define (emit-cmp-res) 
  (emit " sete %al") 
  (emit " movzbl %al, %eax") 
  (emit " sal $~s, %al" bool_bit) 
  (emit " or $~s, %al" bool_f))

;; UNARY PRIMITIVES
(define-primitive (add1 si env arg) (emit " addl $~s, %eax" (immediate-rep 1)))
(define-primitive (sub1 si env arg) (emit " subl $~s, %eax" (immediate-rep 1)))
(define-primitive (char->integer si env arg) (emit " shrl $~s, %eax" (- char_shift fx_shift))) 
(define-primitive (integer->char si env arg) (emit " shll $~s, %eax" 6) (emit " orl $~s, %eax" char_tag)) 
(define-primitive (lognot si env arg) (emit " not %eax") (emit " and $~s, %eax" fx_to_mask)) 
(define-primitive (zero? si env arg) (emit " cmpl $0, %eax") (emit-cmp-res)) 
(define-primitive (fixnum? si env arg) (emit-cmp-msk si env arg fx_mask fx_tag)) 
(define-primitive (null? si env arg) (emit-cmp-msk si env arg char_mask null_list)) 
(define-primitive (boolean? si env arg) (emit-cmp-msk si env arg bool_mask bool_tag)) 
(define-primitive (char? si env arg) (emit-cmp-msk si env arg char_mask char_tag))
(define-primitive (not si env arg) (emit-cmp-msk si env arg char_mask bool_f)) 

;; BINARY PRIMITIVES
(define (emit-bigger? si env shift switched)
  (if switched (begin (emit " subl %eax, ~s(%esp) " si) (emit " movl ~s(%esp), %eax " si)) 
      (emit " subl ~s(%esp), %eax" si))
  (if (not (= 0 shift)) (emit " subl $~s, %eax" shift))
  (emit " and $~s, %eax" fx_neg_mask) 
  (emit " cmpl $~s, %eax" fx_neg_mask) 
  (emit-cmp-res)) 

(define-primitive (+ si env arg1 arg2) (emit " addl ~s(%esp), %eax" si))
(define-primitive (- si env arg1 arg2) (emit " subl %eax, ~s(%esp) " si) (emit " movl ~s(%esp), %eax " si))
(define-primitive (logor si env arg1 arg2) (emit " orl ~s(%esp), %eax" si))
(define-primitive (logand si env arg1 arg2) (emit " and ~s(%esp), %eax" si))
(define-primitive (= si env arg1 arg2) (emit " cmpl ~s(%esp), %eax" si) (emit-cmp-res)) 
(define-primitive (< si env arg1 arg2) (emit-bigger? si env 0 #t))
(define-primitive (> si env arg1 arg2) (emit-bigger? si env 0 #f))
(define-primitive (<= si env arg1 arg2) (emit-bigger? si env 4 #t))
(define-primitive (>= si env arg1 arg2) (emit-bigger? si env 4 #f))
(define-primitive (char<= si env arg1 arg2) (emit-bigger? si env char_dist1 #t))
(define-primitive (char>= si env arg1 arg2) (emit-bigger? si env char_dist1 #f))
(define-primitive (* si env arg1 arg2) (emit " shrl $~s, %eax" fx_shift) (emit " imull ~s(%esp), %eax" si))  

;; CONDITIONAL EXPRESSIONS
(define unique-label 
  (let ([count 0]) 
    (lambda () 
      (let ([L (format "L_~s" count)]) 
	(set! count (+ 1 count)) 
	L)))) 

(define (emit-if si env expr emitter) 
  (let ([alt-label (unique-label)] 
	[end-label (unique-label)]
	[test (cadr expr)]
	[conseq (caddr expr)]
	[altern (cadddr expr)])
    (emit-expr si env test) 
    (emit " cmp $~s, %al" bool_f) 
    (emit " je ~a" alt-label) 
    (emitter si env conseq) 
    (emit " jmp ~a" end-label) 
    (emit "~a:" alt-label) 
    (emitter si env altern) 
    (emit "~a:" end-label)))

;; LOCAL VARIABLES & PROCEDURES (let & letec)
(define (emit-let si env expr * emitter) 
  (define (process-let bindings si new-env)
    (cond [(null? bindings) 
	   (emitter si new-env (let-body expr))] 
	  [else 
	   (let ([b (car bindings)]) 
	     (emit-expr si (if * new-env env) (cadr b)) 
	     (emit-si-save si) 
	     (process-let (cdr bindings) 
			  (next-si si) 
			  (extend-env (car b) si new-env)))])) 
  (process-let (let-bindings expr) si env))

(define (let-bindings expr) (cadr expr))
(define (let-body expr) (cddr expr))
(define (extend-env var si env)  (cons (cons var si) env))
(define (lookup var env) (if (assoc var env) (cdr (assoc var env)) #f))
(define (unique-labels lst) (map (lambda (x) (unique-label)) lst))
(define (make-initial-env lvars labels) (map (lambda (a b) (cons a b)) lvars labels))

(define (emit-variable-ref env var) 
  (cond [(lookup var env) => emit-si-load] 
	[else (error emit-variable-ref "Cannot find the variable reference!" env)])) 

(define (emit-letrec expr) 
  (let* ([bindings (let-bindings expr)] 
	 [lvars (map car bindings)] 
	 [lambdas (map cadr bindings)] 
	 [labels (unique-labels lvars)] 
	 [env (make-initial-env lvars labels)]) 
    (for-each (emit-lambda env) lambdas labels) 
    (emit-scheme-entry (- wordsize) (let-body expr) env))) 

(define (emit-lambda env) 
  (lambda (expr label) 
    (emit-function-header label) 
    (let ([fmls (cadr expr)] 
	  [body (caddr expr)]) 
      (let f ([fmls fmls] [si (- wordsize)] [env env]) 
	(cond [(null? fmls) (begin (emit-tail-expr si env body) (emit " ret"))] 
	      [else 
	       (f (cdr fmls) 
		  (next-si si) 
		  (extend-env (car fmls) si env))])))))

(define (emit-arguments si args env) 
  (unless (null? args) (emit-expr si env (car args)) 	     
	  (emit-si-save si)
	  (emit-arguments (next-si si) (cdr args) env)))

(define (emit-move-arguments si si2 args) 
  (unless (null? args) (emit-movls (list (format "~s(%esp), %eax" si2) (format "%eax, ~s(%esp)" si)))
	  (emit-move-arguments (next-si si) (next-si si2) (cdr args))))

(define (emit-app si env fname args tail)   
  (emit-arguments (next-si si) args env) 
  (if tail (begin (emit-move-arguments (- wordsize) (next-si si) args) 
		  (emit " jmp ~a" fname))
      (begin (emit-adj-base (+ si wordsize)) 
	     (emit-call si fname) 
	     (emit-adj-base (- (+ si wordsize))))))

(define (emit-adj-base x) (emit " addl $~s, %esp" x))
(define (emit-function-header label) (emit ".globl ~s" label) (emit "~s:" label))
(define (emit-scheme-entry si expr env) (emit-function-header 'L_scheme_entry) (emit-expr si env expr) (emit " ret"))
(define (emit-call si label) (emit " call ~s" label))
(define (emit-movls l) (map (lambda (s) (emit " movl ~a" s)) l))
(define (emit-expr si env expr) (emit-hlp si env expr #f)) 
(define (emit-tail-expr si env expr) (emit-hlp si env expr #t))

(define-syntax $and 
  (syntax-rules () 
    [($and) #t] 
    [($and test) test] 
    [($and test test* ...)
     (if test ($and test* ...) #f)]))


;; HEAP OBJECTS
(define (make-heapobj tag) 
  (emit-movls '("%eax, 0(%ebp)" "%eax, %ebx" "%ebp, %eax"))  
  (emit "  orl  $~s, %eax" tag)  
  (emit " addl $11, %ebx")  
  (emit " andl $-8, %ebx")  
  (emit " addl %ebx, %ebp"))

(define (emit-position si size)
  (emit " movl ~s(%esp), %ebx" (next-si si)) 
  (emit " shrl $~s, %ebx" fx_shift) 
  (emit " addl $1, %ebx")
  (emit " imull $~s, %ebx" size)
  (emit " addl ~s(%esp), %ebx" si))

(define-primitive (char= si env arg1 arg2) (emit " cmp ~s(%esp), %eax" si) (emit-cmp-res)) 
(define-primitive (eq? si env arg1 arg2) (emit " cmp ~s(%esp), %eax" si) (emit-cmp-res)) 
(define-primitive (car si env arg) (emit " movl -1(%eax), %eax"))
(define-primitive (cdr si env arg) (emit " movl 3(%eax), %eax"))
(define-primitive (set-car! si env pair v) (emit " movl ~s(%esp), %ebx" si) (emit " movl %eax, -1(%ebx)"))
(define-primitive (set-cdr! si env pair v) (emit " movl ~s(%esp), %ebx" si) (emit " movl %eax, 3(%ebx)"))
(define-primitive (pair? si env arg) (emit-cmp-msk si env arg heapobj_mask pair_tag))

(define-primitive (cons si env arg1 arg2) 
  (emit-movls (list "%eax, 4(%ebp)" (format "~s(%esp), %eax" si) "%eax, 0(%ebp)" "%ebp, %eax"))
  (emit " orl $1, %eax")
  (emit " addl $8, %ebp")) 

(define-primitive (make-vector si env size) (make-heapobj vector_tag))
(define-primitive (vector? si env v) (emit-cmp-msk si env v heapobj_mask vector_tag))
(define-primitive (vector-length si env vec) (emit " movl -5(%eax), %eax"))  
(define-primitive (vector-ref si env vec pos) (emit-position si 4) (emit " movl -5(%ebx), %eax")) 
(define-primitive (vector-set! si env vec pos v) (emit-position si 4) (emit " movl %eax, -5(%ebx)"))

(define-primitive (make-string si env size) (make-heapobj string_tag))
(define-primitive (string? si env v) (emit-cmp-msk si env v heapobj_mask string_tag))
(define-primitive (string-length si env vec) (emit " movl -6(%eax), %eax"))  
(define-primitive (string-ref si env vec pos) (emit-position si 1) (emit " movzbl -1(%ebx), %eax") (emit " shll $~s, %eax" char_shift) (emit " orl $~s, %eax" char_tag)) 
(define-primitive (string-set! si env vec pos v) (emit-position si 1) (emit " shrl $~s, %eax" char_shift) (emit " movb %al, -1(%ebx)"))

;; CLOSURES
(define (sub-exprs expr env)
  (cond [(or (fcall? expr env) 
	     (primcall? expr)
	     (form? expr 'begin)
	     (form? expr 'if)) (cdr expr)]
	[(or (form? expr 'let)
	     (form? expr 'let*)
	     (form? expr 'letrec)) (cons (map cadr (let-bindings expr)) (let-body expr))]
	[else ()]))

(define (add-free-vars expr env)
    (let ([fmls (cdr expr)] 
	  [body (caddr expr)]) 
      (cons 'lambda fmls (find-free-vars fmls body () expr env) body)))

(define (analyse-free-vars expr env)
  (cond [(form? expr 'lambda) (add-free-vars expr)]
	[else (foldr (lambda (v l) (cons (analyse-free-vars v env) l)) (sub-exprs expr env))]))

(define (find-free-vars flms body freev expr env)
  (cond [(variable? expr) (if (member expr flms) freev (cons expr freev))]
	[else (foldr (lambda (v l) (cons (find-free-vars v) l)) (sub-exprs expr env))]))

;; MAIN PROGRAM
(define (emit-hlp si env expr tail) 
  (cond ;[(form? expr '$and) (emit-if si env (expand-syntax (datum->syntax #'$and expr)) (if tail emit-tail-expr emit-expr))] 		
        [(and (not tail) (list? expr) (= 1 (length expr)) (not (fcall? expr env))) (emit-hlp si env (car expr) tail)]
	[(immediate? expr) (emit-immediate expr) (if tail (emit " ret"))] 
	[(variable? expr) (emit-variable-ref env expr) (if tail (emit " ret"))] 
	[(primcall? expr) (emit-primcall si env expr) (if tail (emit " ret"))]  
	[(fcall? expr env) (emit-app si env (lookup (car expr) env) (cdr expr) tail)]
	[(form? expr 'if) (emit-if si env expr (if tail emit-tail-expr emit-expr))] 		
	[(form? expr 'let) (emit-let si env expr #f (if tail emit-tail-expr emit-expr))]
	[(form? expr 'let*) (emit-let si env expr #t (if tail emit-tail-expr emit-expr))]
	[(form? expr 'begin) (emit-hlp si env (cdr expr) tail)]
	[else (begin (emit-hlp si env (car expr) tail) (emit-hlp si env (if (= 1 (length (cdr expr))) (cadr expr) (cdr expr)) tail))]))

(define (emit-program expr) 
  (cond [(expr? expr) (emit-scheme-entry (- wordsize) expr ())]
	[(form? expr 'letrec) (emit-letrec expr)])
  (emit-function-header '_scheme_entry)
  (emit-movls '("4(%esp), %ecx" "%ebx, 4(%ecx)" "%esi, 16(%ecx)" "%edi, 20(%ecx)" "%ebp, 24(%ecx)" "%esp, 28(%ecx)" "12(%esp), %ebp" "8(%esp), %esp"))
  (emit " call ~s" 'L_scheme_entry)
  (emit-movls '("4(%ecx), %ebx" "16(%ecx), %esi" "20(%ecx), %edi" "24(%ecx), %ebp" "28(%ecx), %esp"))
  (emit " ret"))

;; TESTS
;(include "tst/1.4.2.t") 
;(include "tst/rest.t")
;(include "tst/1.1.t") 
;(include "tst/1.2.t") 
;(include "tst/1.3.t") 
;(include "tst/1.4.1.t")
;(include "tst/1.5.t") 
;(include "tst/1.6.t")
;(include "tst/1.7.t") 
;(include "tst/1.8.t")
;(include "tst/1.9.1.t") 
;(include "tst/1.9.2.t")
;(include "tst/1.9.3.t")
(include "tst/2.1.1.t")
(test-with emit-program tests (environment '(scheme)))
