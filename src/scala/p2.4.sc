//p2.4 uncurry
def uncurry[A, B, C](f: A => B => C): (A, B) => C = f(_)(_)
