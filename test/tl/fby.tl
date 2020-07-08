fun prev.D(X)  = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
dim d <- 0
var x = 0 fby.d x+1
evalExpr x @ [d <- -2 .. 2]
