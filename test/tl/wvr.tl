fun index.D = #D
fun next.D(X) = X [D <- #D + 1]
fun prev.D(X) = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun wvr.D(X,Y) = X [D <- t]
  where
    var t = u fby.D u [D <- t+1]
    var u = if Y then #D else next.D(u)
  end
fun even(X) = X % 2 == 0
dim d <- 0
var z = index.d wvr.d even(index.d)
evalExpr z @ [d <- 0 .. 7]
