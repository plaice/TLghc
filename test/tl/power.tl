fun lParent.d(X) = X [d <- #d / 2 ]
fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun alt.d(X,Y) = if #d %2 == 0 then X else Y
fun cby.d(X,Y) = if #d < 1 then X else Y
fun power(n,X) = P
where
  dim d <- n
  var P = 1 cby.d
          ((lParent.d(P) * lParent.d(P)) alt.d
           (X * prev.d(P)))
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
