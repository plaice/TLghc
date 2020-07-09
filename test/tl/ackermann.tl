fun index.d = #d
fun prev.d(X) = X [d <- #d - 1]
fun next.d(X) = X [d <- #d + 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun ackermann(m,n) = F
where
  dim dm <- m
  dim dn <- n
  var F = next.dn(index.dn) fby.dm
          (next.dn(F) fby.dn (F [dn <- next.dm(F)]))
end
evalExpr ackermann(0,0) @ []
evalExpr ackermann(0,1) @ []
evalExpr ackermann(0,2) @ []
evalExpr ackermann(1,0) @ []
evalExpr ackermann(1,1) @ []
evalExpr ackermann(1,2) @ []
evalExpr ackermann(1,3) @ []
evalExpr ackermann(2,0) @ []
evalExpr ackermann(2,1) @ []
evalExpr ackermann(2,2) @ []
evalExpr ackermann(2,3) @ []
evalExpr ackermann(3,0) @ []
evalExpr ackermann(3,1) @ []
evalExpr ackermann(3,2) @ []
evalExpr ackermann(3,3) @ []
evalExpr ackermann(4,0) @ []
