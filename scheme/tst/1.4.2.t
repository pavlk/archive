(add-tests-with-string-output "$and"
			      [($and) => "#t\n"]
			      [($and #t) => "#t\n"]
			      [($and #f) => "#f\n"]
			      [($and #t #f)  => "#f\n"]
			      [($and #t #t)  => "#t\n"]
			      [($and #t (char? 12)) => "#f\n"]
			      [($and #t #t (null? empty) #t #t) => "#t\n"]
			      [($and #f #t #t) => "#f\n"]
			      )
