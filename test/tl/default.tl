fun index.D = #D
fun default.D(N,X,Y) = if #D < 1 || #D > N then X else Y
dim d <- 0
evalExpr default.d(0,1,index.d) @ [d <- 0 .. 4]
evalExpr default.d(1,1,index.d) @ [d <- 0 .. 4]
evalExpr default.d(2,1,index.d) @ [d <- 0 .. 4]
evalExpr default.d(3,1,index.d) @ [d <- 0 .. 4]
evalExpr default.d(4,1,index.d) @ [d <- 0 .. 5]
evalExpr default.d(5,1,index.d) @ [d <- 0 .. 6]
