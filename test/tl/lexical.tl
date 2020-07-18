var result = v
where
  var a = 5
  fun f(x) = x + a
  var v = w + b
  where
    var a = 7
    var b = a - 7
    var w = f(3)
  end
end
evalExpr result @ []
