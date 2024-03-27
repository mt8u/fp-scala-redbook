object MyProgram:         
  def abs(n: Int): Int = 
    if n < 0 then -n    
    else n

  def factorial(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, n * acc)
    go(n, 1)

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
 
  private def formatFactorial(n: Int) =
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  def formatResult(name: String, n: Int, f: Int => Int) =
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
 
  @main def printAbsAndFactorial: Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
