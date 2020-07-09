fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
dim d <- 0
var X = 0 fby.d X+1
evalExpr X @ [d <- -2 .. 2]
