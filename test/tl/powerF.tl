fun lParent.d(X) = X [d <- #d / 2]
fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun alt.d(X,Y) = if #d % 2 == 0 then X else Y
fun cby.d(X,Y) = if #d < 1 then X else Y
fun powerF(n) = P
where
  dim d <- n
  {- Parser still cannot parse prev.d(P)(X) -}
  var P = (fn(X) -> 1) cby.d
          ((fn(X) -> LP(X) * LP(X)) alt.d
           (fn(X) -> X * PP(X)))
  var LP = lParent.d(P)
  var PP = prev.d(P)
end
var power0 = powerF(0)
var power1 = powerF(1)
var power2 = powerF(2)
var power3 = powerF(3)
evalExpr power0(1) @ []
evalExpr power1(1) @ []
evalExpr power2(1) @ []
evalExpr power3(1) @ []
evalExpr power0(2) @ []
evalExpr power1(2) @ []
evalExpr power2(2) @ []
evalExpr power3(2) @ []
evalExpr power0(3) @ []
evalExpr power1(3) @ []
evalExpr power2(3) @ []
evalExpr power3(3) @ []
