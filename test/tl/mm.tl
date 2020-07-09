fun index.d = #d
fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun rotate.d1.d2(X) = X [ d1 <- #d2 ]
fun sum.d(N,X) = Y [ d <- N]
where
  var Y = 0 fby.d (X + Y)
end
fun mm.drow.dcol(k,X,Y) = W
where
  dim d <- 0
  var Xp = rotate.drow.d(X)
  var Yp = rotate.dcol.d(Y)
  var Z = Xp * Yp
  var W = sum.d(k,Z)
end
dim drow <- 0
dim dcol <- 0
var X = index.drow + index.dcol
evalExpr X @ [drow <- 0..2, dcol <- 0..2]
evalExpr mm.drow.dcol(3,X,X) @ [drow <- 0..2, dcol <- 0..2]
