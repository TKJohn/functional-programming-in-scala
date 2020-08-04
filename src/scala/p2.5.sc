//p2.5 compose
def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))
