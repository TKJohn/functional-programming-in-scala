// p2.1 fibonacci
def fib(n: Int): Long = {
  @annotation.tailrec
  def go(n: Int, a: Long, b: Long): Long = {
    if (n == 1) b
    else go(n - 1, b, a + b)
  }

  go(n, 0, 1)
}
