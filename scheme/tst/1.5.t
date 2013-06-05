(add-tests-with-string-output "+"
  [(+ 1 2) => "3\n"]
  [(+ 1 -2) => "-1\n"]
  [(+ -1 2) => "1\n"]
  [(+ -1 -2) => "-3\n"]
  [(+ 128 128) => "256\n"]
  [(+ 493799 5863) => "499662\n"]
  [(+ 5368 1035) => "6403\n"]
  [(+ 536870911 -1) => "536870910\n"]
  [(+ 536870910 1) => "536870911\n"]
  [(+ -536870912 1) => "-536870911\n"]
  [(+ -536870911 -1) => "-536870912\n"]
  [(+ 536870911 -536870912) => "-1\n"]
  [(+ 1 (+ 2 3)) => "6\n"]
  [(+ 1 (+ 2 -3)) => "0\n"]
  [(+ 1 (+ -2 3)) => "2\n"]
  [(+ 1 (+ -2 -3)) => "-4\n"]
  [(+ -1 (+ 2 3)) => "4\n"]
  [(+ -1 (+ 2 -3)) => "-2\n"]
  [(+ -1 (+ -2 3)) => "0\n"]
  [(+ -1 (+ -2 -3)) => "-6\n"]
  [(+ (+ 1 2) 3) => "6\n"]
  [(+ (+ 1 2) -3) => "0\n"]
  [(+ (+ 1 -2) 3) => "2\n"]
  [(+ (+ 1 -2) -3) => "-4\n"]
  [(+ (+ -1 2) 3) => "4\n"]
  [(+ (+ -1 2) -3) => "-2\n"]
  [(+ (+ -1 -2) 3) => "0\n"]
  [(+ (+ -1 -2) -3) => "-6\n"]
  [(+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) => "45\n"]
  [(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 9)))))))) => "45\n"]
)
 
(add-tests-with-string-output "-"
  [(- 1 2) => "-1\n"]
  [(- 1 -2) => "3\n"]
  [(- -1 2) => "-3\n"]
  [(- -1 -2) => "1\n"]
  [(- 536870910 123) => "536870787\n"]
  [(- 256 128) => "128\n"]
  [(- 812 22) => "790\n"]
  [(- 536870910 -1) => "536870911\n"]
  [(- 536870911 1) => "536870910\n"]
  [(- -536870911 1) => "-536870912\n"]
  [(- -536870912 -1) => "-536870911\n"]
  [(- 1 536870911) => "-536870910\n"]
  [(- -1 536870911) => "-536870912\n"]
  [(- 1 -536870910) => "536870911\n"]
  [(- -1 -536870912) => "536870911\n"]
  [(- 536870911 536870911) => "0\n"]
  ;[(- 536870911 -536870912) => "-1\n"]
  [(- -536870911 -536870912) => "1\n"]
  [(- 1 (- 2 3)) => "2\n"]
  [(- 1 (- 2 -3)) => "-4\n"]
  [(- 1 (- -2 3)) => "6\n"]
  [(- 1 (- -2 -3)) => "0\n"]
  [(- -1 (- 2 3)) => "0\n"]
  [(- -1 (- 2 -3)) => "-6\n"]
  [(- -1 (- -2 3)) => "4\n"]
  [(- -1 (- -2 -3)) => "-2\n"]
  [(- 0 (- -2 -3)) => "-1\n"]
  [(- (- 1 2) 3) => "-4\n"]
  [(- (- 1 2) -3) => "2\n"]
  [(- (- 1 -2) 3) => "0\n"]
  [(- (- 1 -2) -3) => "6\n"]
  [(- (- -1 2) 3) => "-6\n"]
  [(- (- -1 2) -3) => "0\n"]
  [(- (- -1 -2) 3) => "-2\n"]
  [(- (- -1 -2) -3) => "4\n"]
  [(- (- (- (- (- (- (- (- 1 2) 3) 4) 5) 6) 7) 8) 9) => "-43\n"]
  [(- 1 (- 2 (- 3 (- 4 (- 5 (- 6 (- 7 (- 8 9)))))))) => "5\n"]
)

(add-tests-with-string-output "*"
  [(* 2 3) => "6\n"]
  [(* 2 -3) => "-6\n"]
  [(* -2 3) => "-6\n"]
  [(* -2 -3) => "6\n"]
  [(* 536870911 1) => "536870911\n"]
  [(* 536870911 -1) => "-536870911\n"]
  [(* -536870912 1) => "-536870912\n"]
  [(* -536870911 -1) => "536870911\n"]
  [(* 2 (* 3 4)) => "24\n"]
  [(* (* 2 3) 4) => "24\n"]
  [(* (* (* (* (* 2 3) 4) 5) 6) 7) => "5040\n"]
  [(* 2 (* 3 (* 4 (* 5 (* 6 7))))) => "5040\n"]
)

(add-tests-with-string-output "="
  [(= 12 13) => "#f\n"]
  [(= 12 12) => "#t\n"]
  [(= 16 (+ 13 3)) => "#t\n"]
  [(= 16 (+ 13 13)) => "#f\n"]
  [(= (+ 13 3) 16) => "#t\n"]
  [(= (+ 13 13) 16) => "#f\n"]
)

(add-tests-with-string-output "<"
  [(< 12 13) => "#t\n"]
  [(< 12 12) => "#f\n"]
  [(< 13 12) => "#f\n"]
  [(< 16 (+ 13 1)) => "#f\n"]
  [(< 16 (+ 13 3)) => "#f\n"]
  [(< 16 (+ 13 13)) => "#t\n"]
  [(< (+ 13 1) 16) => "#t\n"]
  [(< (+ 13 3) 16) => "#f\n"]
  [(< (+ 13 13) 16) => "#f\n"]
) 

(add-tests-with-string-output "<="
  [(<= 12 13) => "#t\n"]
  [(<= 12 12) => "#t\n"]
  [(<= 13 12) => "#f\n"]
  [(<= 16 (+ 13 1)) => "#f\n"]
  [(<= 16 (+ 13 3)) => "#t\n"]
  [(<= 16 (+ 13 13)) => "#t\n"]
  [(<= (+ 13 1) 16) => "#t\n"]
  [(<= (+ 13 3) 16) => "#t\n"]
  [(<= (+ 13 13) 16) => "#f\n"]
)

(add-tests-with-string-output ">"
  [(> 12 13) => "#f\n"]
  [(> 12 12) => "#f\n"]
  [(> 13 12) => "#t\n"]
  [(> 16 (+ 13 1)) => "#t\n"]
  [(> 16 (+ 13 3)) => "#f\n"]
  [(> 16 (+ 13 13)) => "#f\n"]
  [(> (+ 13 1) 16) => "#f\n"]
  [(> (+ 13 3) 16) => "#f\n"]
  [(> (+ 13 13) 16) => "#t\n"]
)

(add-tests-with-string-output ">="
  [(>= 12 13) => "#f\n"]
  [(>= 12 12) => "#t\n"]
  [(>= 13 12) => "#t\n"]
  [(>= 16 (+ 13 1)) => "#t\n"]
  [(>= 16 (+ 13 3)) => "#t\n"]
  [(>= 16 (+ 13 13)) => "#f\n"]
  [(>= (+ 13 1) 16) => "#f\n"]
  [(>= (+ 13 3) 16) => "#t\n"]
  [(>= (+ 13 13) 16) => "#t\n"]
)

(add-tests-with-string-output "if"
  [(if (= 12 13) 12 13) => "13\n"]
  [(if (= 12 12) 13 14) => "13\n"]
  [(if (< 12 13) 12 13) => "12\n"]
  [(if (< 12 12) 13 14) => "14\n"]
  [(if (< 13 12) 13 14) => "14\n"]
  [(if (<= 12 13) 12 13) => "12\n"]
  [(if (<= 12 12) 12 13) => "12\n"]
  [(if (<= 13 12) 13 14) => "14\n"]
  [(if (> 12 13) 12 13) => "13\n"]
  [(if (> 12 12) 12 13) => "13\n"]
  [(if (> 13 12) 13 14) => "13\n"]
  [(if (>= 12 13) 12 13) => "13\n"]
  [(if (>= 12 12) 12 13) => "12\n"]
  [(if (>= 13 12) 13 14) => "13\n"]
)

(add-tests-with-string-output "binary primitives"
  [(+ (+ 1 2) (+ 3 4)) => "10\n"]
  [(+ (+ 1 2) (+ 3 -4)) => "2\n"]
  [(+ (+ 1 2) (+ -3 4)) => "4\n"]
  [(+ (+ 1 2) (+ -3 -4)) => "-4\n"]
  [(+ (+ 1 -2) (+ 3 4)) => "6\n"]
  [(+ (+ 1 -2) (+ 3 -4)) => "-2\n"]
  [(+ (+ 1 -2) (+ -3 4)) => "0\n"]
  [(+ (+ 1 -2) (+ -3 -4)) => "-8\n"]
  [(+ (+ -1 2) (+ 3 4)) => "8\n"]
  [(+ (+ -1 2) (+ 3 -4)) => "0\n"]
  [(+ (+ -1 2) (+ -3 4)) => "2\n"]
  [(+ (+ -1 2) (+ -3 -4)) => "-6\n"]
  [(+ (+ -1 -2) (+ 3 4)) => "4\n"]
  [(+ (+ -1 -2) (+ 3 -4)) => "-4\n"]
  [(+ (+ -1 -2) (+ -3 4)) => "-2\n"]
  [(+ (+ -1 -2) (+ -3 -4)) => "-10\n"]
  [(+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) => "45\n"]
  [(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 9)))))))) => "45\n"]
  [(+ (+ (+ (+ 1 2) (+ 3 4)) (+ (+ 5 6) (+ 7 8)))
        (+ (+ (+ 9 10) (+ 11 12)) (+ (+ 13 14) (+ 15 16))))  => "136\n"]
  [(- (- 1 2) (- 3 4)) => "0\n"]
  [(- (- 1 2) (- 3 -4)) => "-8\n"]
  [(- (- 1 2) (- -3 4)) => "6\n"]
  [(- (- 1 2) (- -3 -4)) => "-2\n"]
  [(- (- 1 -2) (- 3 4)) => "4\n"]
  [(- (- 1 -2) (- 3 -4)) => "-4\n"]
  [(- (- 1 -2) (- -3 4)) => "10\n"]
  [(- (- 1 -2) (- -3 -4)) => "2\n"]
  [(- (- -1 2) (- 3 4)) => "-2\n"]
  [(- (- -1 2) (- 3 -4)) => "-10\n"]
  [(- (- -1 2) (- -3 4)) => "4\n"]
  [(- (- -1 2) (- -3 -4)) => "-4\n"]
  [(- (- -1 -2) (- 3 4)) => "2\n"]
  [(- (- -1 -2) (- 3 -4)) => "-6\n"]
  [(- (- -1 -2) (- -3 4)) => "8\n"]
  [(- (- -1 -2) (- -3 -4)) => "0\n"]
  [(- (- (- (- (- (- (- (- 1 2) 3) 4) 5) 6) 7) 8) 9) => "-43\n"]
  [(- 1 (- 2 (- 3 (- 4 (- 5 (- 6 (- 7 (- 8 9)))))))) => "5\n"]
  [(- (- (- (- 1 2) (- 3 4)) (- (- 5 6) (- 7 8)))
        (- (- (- 9 10) (- 11 12)) (- (- 13 14) (- 15 16))))  => "0\n"]
  [(* (* (* (* 2 3) (* 4 5)) (* (* 6 7) (* 8 9)))
        (* (* (* 2 3) (* 2 3)) (* (* 2 3) (* 2 3))))  => "470292480\n"]
  [(= (+ 13 3) (+ 10 6)) => "#t\n"]
  [(= (+ 13 0) (+ 10 6)) => "#f\n"]
  [(= (+ 12 1) (+ -12 -1)) => "#f\n"]
  [(< (+ 10 6) (+ 13 1)) => "#f\n"]
  [(< (+ 10 6) (+ 13 3)) => "#f\n"]
  [(< (+ 10 6) (+ 13 31)) => "#t\n"]
  [(< (+ 12 1) (+ -12 -1)) => "#f\n"]
  [(< (+ -12 -1) (+ 12 1)) => "#t\n"]
  [(<= (+ 10 6) (+ 13 1)) => "#f\n"]
  [(<= (+ 10 6) (+ 13 3)) => "#t\n"]
  [(<= (+ 10 6) (+ 13 31)) => "#t\n"]
  [(<= (+ 12 1) (+ -12 -1)) => "#f\n"]
  [(<= (+ -12 -1) (+ 12 1)) => "#t\n"]
  [(> (+ 10 6) (+ 13 1)) => "#t\n"]
  [(> (+ 10 6) (+ 13 3)) => "#f\n"]
  [(> (+ 10 6) (+ 13 31)) => "#f\n"]
  [(> (+ 12 1) (+ -12 -1)) => "#t\n"]
  [(> (+ -12 -1) (+ 12 1)) => "#f\n"]
  [(>= (+ 10 6) (+ 13 1)) => "#t\n"]
  [(>= (+ 10 6) (+ 13 3)) => "#t\n"]
  [(>= (+ 10 6) (+ 13 31)) => "#f\n"]
  [(>= (+ 12 1) (+ -12 -1)) => "#t\n"]
  [(>= (+ -12 -1) (+ 12 1)) => "#f\n"]
)
