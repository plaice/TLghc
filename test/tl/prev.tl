fun index.D = #D
fun prev.D(X) = X [D <- #D - 1]
dim d <- 0
evalExpr prev.d(index.d) @ [d <- -2 .. 2]
