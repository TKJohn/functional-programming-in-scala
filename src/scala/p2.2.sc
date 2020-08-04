// p2.2 isSorted

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int, as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (n < 2) true
    else if (!ordered(as(n - 2), as(n - 1))) false
    else go(n - 1, as, ordered)
  }

  go(as.length, as, ordered)
}
