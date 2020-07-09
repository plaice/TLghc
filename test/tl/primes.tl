fun index.D = #D
fun first.D(X) = X [D <- 0]
fun next.D(X) = X [D <- #D + 1]
fun prev.D(X) = X [D <- #D - 1]
fun fby.D(X,Y) = if #D < 1 then X else prev.D(Y)
fun wvr.D(X,Y) = X [D <- t]
  where
    var t = u fby.D u [D <- t+1]
    var u = if Y then #D else next.D(u)
  end
fun primes.D = f
  where
    dim z <- 0
    var f = #z + 2 fby.D (f wvr.z (f % first.z(f) /= 0))
  end
dim d <- 0
evalExpr primes.d @ [d <- 0 .. 7]
