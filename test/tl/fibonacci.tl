fun lParent.d(X) = X [d <- #d / 2 ]
fun rParent.d(X) = X [d <- #d / 2 + 1]
fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun alt.d(X,Y) = if #d %2 == 0 then X else Y
fun cby3.d(X,Y) = if #d < 3 then X else Y
fun fibonacci(n) = F
where
  dim d <- n
  var F = (0 fby.d 1) cby3.d
          ((lParent.d(F) * (rParent.d(F) * 2 - lParent.d(F))) alt.d
           (lParent.d(F) * lParent.d(F) + rParent.d(F) * rParent.d(F)))
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
