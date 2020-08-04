// p2.3 curry
def curry[A, B, C](f: (A, B) => C): A => B => C =
  a => f(a, _)

