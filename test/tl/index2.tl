fun index2.d1.d2 = #d1 + #d2
dim d1 <- 0
dim d2 <- 0
evalExpr index2.d1.d2 @ [d1 <- -2 .. 2, d2 <- -2 .. 2]
