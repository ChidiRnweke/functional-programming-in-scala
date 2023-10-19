def factorial(n: Int): Int =
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if n <= 0 then acc else go(n - 1, n * acc)
  go(n, 1)

def myFactorial(n: Int): Int =
  (1 to n).reduce((x, y) => x * y)

@main def factorialTest: Unit =
  val myFactorialResult = myFactorial(5)
  val bookFatorial = factorial(5)
  println(s"My factorial's result is $myFactorialResult")
  println(s"The book's factorial is $bookFatorial")
