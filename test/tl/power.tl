fun lParent.D(X) = X [D <- #D / 2 ]
fun prev.D(X) = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun alt.D(X,Y) = if #D %2 == 0 then X else Y
fun cby.D(X,Y) = if #D < 1 then X else Y
fun power(N,X) = p
where
  dim d <- N
  var p = 1 cby.d
          ((lParent.d(p) * lParent.d(p)) alt.d
           (X * prev.d(p)))
end
evalExpr power(0,1) @ []
evalExpr power(1,1) @ []
evalExpr power(2,1) @ []
evalExpr power(3,1) @ []
evalExpr power(0,2) @ []
evalExpr power(1,2) @ []
evalExpr power(2,2) @ []
evalExpr power(3,2) @ []
evalExpr power(0,3) @ []
evalExpr power(1,3) @ []
evalExpr power(2,3) @ []
evalExpr power(3,3) @ []
