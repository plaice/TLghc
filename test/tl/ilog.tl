fun prev.D(X)  = X [D <- #D - 1]
fun rParent.D(X)  = X [D <- #D / 2 + 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun ilog(N) = il
where
  dim d <- N
  var il = 0 fby.d (1 fby.d (1 + rParent.d(il)))
end
evalExpr ilog(0) @ []
evalExpr ilog(1) @ []
evalExpr ilog(2) @ []
evalExpr ilog(3) @ []
evalExpr ilog(4) @ []
evalExpr ilog(5) @ []
evalExpr ilog(6) @ []
evalExpr ilog(7) @ []
evalExpr ilog(8) @ []
