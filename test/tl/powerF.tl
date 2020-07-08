fun lParent.D(X)  = X [D <- #D / 2 ]
fun prev.D(X)  = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun alt.D(X,Y) = if #D %2 == 0 then X else Y
fun cby.D(X,Y) = if #D < 1 then X else Y
fun powerF(N) = p
where
  dim d <- N
  {- Parser still cannot parse prev.d(p)(x) -}
  var p = (fn(x) -> 1) cby.d
          ((fn(x) -> lp(x) * lp(x)) alt.d
           (fn(x) -> x * pp(x)))
  var lp = lParent.d(p)
  var pp = prev.d(p)
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
