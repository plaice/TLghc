fun prev.d(X) = X [d <- #d - 1]
fun rParent.d(X) = X [d <- #d / 2 + 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun ilog(n) = il
where
  dim d <- n
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
