fun next.D(X)  = X [D <- #D + 1]
fun ybf.D(X,Y) = if #D > -1 then Y else next.D(X)
dim d <- 0
var x = x-1 ybf.d 0
evalExpr x @ [d <- -2 .. 2]
