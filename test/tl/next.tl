fun index.D = #D
fun next.D(X) = X [D <- #D + 1]
dim d  <- 0
evalExpr next.d(index.d) @ [d <- -2 .. 2]
