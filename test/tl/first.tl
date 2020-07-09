fun index.D = #D
fun first.D(X) = X [D <- 0]
dim d <- 0
evalExpr first.d(index.d) @ [d <- -2 .. 2]
