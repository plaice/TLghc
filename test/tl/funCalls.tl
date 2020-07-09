dim d <- 0
dim e <- 0
evalExpr (fn(x) -> #d)(1) @ [d <- -2 .. 2]
evalExpr (fn(x) -> #d + x)(#d + #e) @ [d <- -2 .. 2, e <- -2 .. 2]
evalExpr ((fn(x) -> #d + x)[d <- 3])(#d + #e) @ [d <- -2 .. 2, e <- -2 .. 2]
