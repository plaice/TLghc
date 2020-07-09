fun next.d(X) = X [d <- #d + 1]
fun ybf.d(X,Y) = if #d > -1 then Y else next.d(X)
dim d <- 0
var X = X-1 ybf.d 0
evalExpr X @ [d <- -2 .. 2]
