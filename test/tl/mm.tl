fun index.D = #D
fun prev.D(X) = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun rotate.D1.D2(X) = X [ D1 <- #D2 ]
fun sum.D(N,X) = y [ D <- N]
where
  var y = 0 fby.D (X + y)
end
fun mm.Drow.Dcol(K,X,Y) = w
where
  dim d <- 0
  var xp = rotate.Drow.d(X)
  var yp = rotate.Dcol.d(Y)
  var z = xp * yp
  var w = sum.d(K,z)
end
dim drow <- 0
dim dcol <- 0
var x = index.drow + index.dcol
evalExpr x @ [drow <- 0..2, dcol <- 0..2]
evalExpr mm.drow.dcol(3,x,x) @ [drow <- 0..2, dcol <- 0..2]
