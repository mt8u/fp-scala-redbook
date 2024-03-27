def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
  @annotation.tailrec
  def loop(n: Int): Boolean =
    if (n >= as.length - 1) then true
    else if gt(as(n +1), as(n)) then false
    else loop(n + 1)
  loop(0)
