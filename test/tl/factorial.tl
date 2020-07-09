fun index.d = #d
fun rParent.d(X) = X [d <- #d / 2 + 1]
fun lChild.d(X) = X [d <- #d * 2]
fun rChild.d(X) = X [d <- #d * 2 + 1]
fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun ilog(n) = il
where
  dim d <- n
  var il = 0 fby.d (1 fby.d (1 + rParent.d(il)))
end
fun default.d(n,X,Y) = if #d < 1 || #d > n then X else Y
fun factorial(n) = f
where
  dim d1 <- ilog(n)
  dim d2 <- 0
  var f = default.d2(n, 1, index.d2)
          fby.d1 (lChild.d2(f) * rChild.d2(f))
end
evalExpr factorial(0) @ []
evalExpr factorial(1) @ []
evalExpr factorial(2) @ []
evalExpr factorial(3) @ []
evalExpr factorial(4) @ []
evalExpr factorial(5) @ []
evalExpr factorial(6) @ []
evalExpr factorial(7) @ []
evalExpr factorial(8) @ []
evalExpr factorial(100) @ []
evalExpr factorial(1000) @ []
