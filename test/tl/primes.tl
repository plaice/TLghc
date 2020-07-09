fun index.d = #d
fun first.d(X) = X [d <- 0]
fun next.d(X) = X [d <- #d + 1]
fun prev.d(X) = X [d <- #d - 1]
fun fby.d(X,Y) = if #d < 1 then X else prev.d(Y)
fun wvr.d(X,Y) = X [d <- T]
  where
    var T = U fby.d U [d <- T+1]
    var U = if Y then #d else next.d(U)
  end
fun primes.d = F
  where
    dim z <- 0
    var F = #z + 2 fby.d (F wvr.z (F % first.z(F) /= 0))
  end
dim d <- 0
evalExpr primes.d @ [d <- 0 .. 7]
