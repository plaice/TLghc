fun index.d = #d
fun first.d(X) = X [d <- 0]
dim d <- 0
evalExpr first.d(index.d) @ [d <- -2 .. 2]
