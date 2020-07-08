fun lParent.D(X)  = X [D <- #D / 2 ]
fun rParent.D(X)  = X [D <- #D / 2 + 1]
fun even.D = (#D % 2) == 0
fun prev.D(X)  = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun cby.D(N,X,Y) = if #D < N then X else Y
fun fibonacci(N) = f
where
  dim d <- N
  var f = cby.d (3,
                 0 fby.d 1,
                 if even.d
                 then lParent.d(f) * (rParent.d(f) * 2 - lParent.d(f))
                 else lParent.d(f) * lParent.d(f) +
                      rParent.d(f) * rParent.d(f))
end
evalExpr fibonacci(0) @ []
evalExpr fibonacci(1) @ []
evalExpr fibonacci(2) @ []
evalExpr fibonacci(3) @ []
evalExpr fibonacci(4) @ []
evalExpr fibonacci(5) @ []
evalExpr fibonacci(6) @ []
evalExpr fibonacci(7) @ []
evalExpr fibonacci(8) @ []
evalExpr fibonacci(100) @ []
evalExpr fibonacci(1000) @ []
