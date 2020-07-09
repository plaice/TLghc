fun index.d = #d
fun next.d(X) = X [d <- #d + 1]
fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun wvr.d(X,Y) = X [d <- T]
  where
    var T = U fby.d U [d <- T+1]
    var U = if Y then #d else next.d(U)
  end
fun even(X) = X % 2 == 0
dim d <- 0
var Z = index.d wvr.d even(index.d)
evalExpr Z @ [d <- 0 .. 7]
