(add-tests-with-string-output "<"
			      [(< #\a #\b) => "#t\n"]
			      [(< #\d #\d) => "#f\n"]
			      [(< #\b #\a) => "#f\n"]
			      )

(add-tests-with-string-output "char<="
			      [(char<= #\a #\b) => "#t\n"]
			      [(char<= #\d #\d) => "#t\n"]
			      [(char<= #\t #\t) => "#t\n"]
			      [(char<= #\b #\a) => "#f\n"]
			      )

(add-tests-with-string-output "char>="
			      [(char>= #\a #\b) => "#f\n"]
			      [(char>= #\d #\d) => "#t\n"]
			      [(char>= #\b #\a) => "#t\n"]
			      [(char<= #\D #\D) => "#t\n"]
			      [(char<= #\t #\t) => "#t\n"]
			      )

(add-tests-with-string-output "logand and logor"
			      [(logor 3 16) => "19\n"]
			      [(logor 3 5)  => "7\n"]
			      [(logor 3 7)  => "7\n"]
			      [(lognot (logor (lognot 7) 1)) => "6\n"]
			      [(lognot (logor 1 (lognot 7))) => "6\n"]
			      [(logand 3 7) => "3\n"]
			      [(logand 3 5) => "1\n"]
			      [(logand 2346 (lognot 2346)) => "0\n"]
			      [(logand (lognot 2346) 2346) => "0\n"]
			      [(logand 2376 2376) => "2376\n"]
			      )

(add-tests-with-string-output "lognot"
			      [(lognot -7) => "6\n"]
			      [(lognot (logor (lognot 7) 1)) => "6\n"]
			      [(lognot (logor (lognot 7) (lognot 2))) => "2\n"]
			      [(logand (lognot (lognot 12)) (lognot (lognot 12))) => "12\n"]
			      [(lognot (logor (lognot 7) 1)) => "6\n"]
			      [(lognot (logor (lognot 7) (lognot 2))) => "2\n"]
			      [(logand (lognot (lognot 12)) (lognot (lognot 12))) => "12\n"]
			      [(lognot 0) => "-1\n"]
			      [(lognot -1) => "0\n"]
			      [(lognot 1) => "-2\n"]
			      [(lognot -2) => "1\n"]
			      [(lognot 536870911) => "-536870912\n"]
			      [(lognot -536870912) => "536870911\n"]
			      [(lognot (lognot 237463)) => "237463\n"]
			      )
