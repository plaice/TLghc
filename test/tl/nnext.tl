fun index.D = #D
fun next.D(X) = X [D <- #D + 1]
fun prev.D(X) = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun nnext(N) = nexts
where
  fun nexts.d(x) = x [d <- #d + N]
end
fun nnextF(N) = nexts
where
  dim dN <- N
  var nexts = (fn.d(x) -> x) fby.dN
              (fn.d(x) -> next.d(nexts.d(x)))
end
dim d <- 0
var next0 = nnext(0)
var next1 = nnext(1)
var next2 = nnext(2)
var next3 = nnext(3)
var next4 = nnext(4)
var next5 = nnext(5)
var nextF0 = nnextF(0)
var nextF1 = nnextF(1)
var nextF2 = nnextF(2)
var nextF3 = nnextF(3)
var nextF4 = nnextF(4)
var nextF5 = nnextF(5)
evalExpr index.d @ [d <- -2 .. 2]
evalExpr next0.d(index.d) @ [d <- -2 .. 2]
evalExpr next1.d(index.d) @ [d <- -2 .. 2]
evalExpr next2.d(index.d) @ [d <- -2 .. 2]
evalExpr next3.d(index.d) @ [d <- -2 .. 2]
evalExpr next4.d(index.d) @ [d <- -2 .. 2]
evalExpr next5.d(index.d) @ [d <- -2 .. 2]
evalExpr nextF0.d(index.d) @ [d <- -2 .. 2]
evalExpr nextF1.d(index.d) @ [d <- -2 .. 2]
evalExpr nextF2.d(index.d) @ [d <- -2 .. 2]
evalExpr nextF3.d(index.d) @ [d <- -2 .. 2]
evalExpr nextF4.d(index.d) @ [d <- -2 .. 2]
evalExpr nextF5.d(index.d) @ [d <- -2 .. 2]
