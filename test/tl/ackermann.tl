fun index.D = #D
fun prev.D(X) = X [D <- #D - 1]
fun next.D(X) = X [D <- #D + 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun ackermann(M,N) = f
where
  dim dm <- M
  dim dn <- N
  var f = next.dn(index.dn) fby.dm
          (next.dn(f) fby.dn (f [dn <- next.dm(f)]))
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
