def fib(n: Int): Int =
  @annotation.tailrec
  def go(n: Int, previous: Int, current: Int): Int =
    if n <= 0 then previous
    else go(n - 1, current, previous + current)
  go(n, 0, 1)

   
