fun index.d = #d
fun prev.d(X) = X [d <- #d - 1]
dim d <- 0
evalExpr prev.d(index.d) @ [d <- -2 .. 2]
