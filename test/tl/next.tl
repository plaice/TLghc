fun index.d = #d
fun next.d(X) = X [d <- #d + 1]
dim d <- 0
evalExpr next.d(index.d) @ [d <- -2 .. 2]
