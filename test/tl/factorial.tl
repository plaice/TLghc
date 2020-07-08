fun index.D = #D
fun rParent.D(X)  = X [D <- #D / 2 + 1]
fun lChild.D(X)  = X [D <- #D * 2]
fun rChild.D(X)  = X [D <- #D * 2 + 1]
fun prev.D(X)  = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun ilog(N) = il
where
  dim d <- N
  var il = 0 fby.d (1 fby.d (1 + rParent.d(il)))
end
fun default.D(N,X,Y) = if #D < 1 then X else if #D > N then X else Y
fun factorial(N) = f
where
  dim d1 <- ilog(N)
  dim d2 <- 0
  var f = default.d2(N, 1, index.d2)
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
