<?php

$r = new R;
$r->init();
$r->parseEval("
	fibonacci <- function(n) {
		fib <- numeric(n)
			fib [1:2] <- 1
			for (i in 3:n) {
				fib[i] <- fib[i-1] + fib[i-2]
			}
		return(fib[n])
	}
");

var_dump($r->fibonacci(40));

/*
function f($n) {
	if ($n < 2)
		return 1;
	else
		return f($n-2) + f($n-1);
}

echo f(40);
*/
?>
