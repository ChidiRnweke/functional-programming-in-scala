import scala.annotation.tailrec

def fibonacci(n: Int): Int =
  n match
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)

def fib(n: Int): Int =
  @tailrec
  def go(calls: Int, prev: Int, prev2: Int): Int =
    if calls == n then prev + prev2
    else go(calls + 1, prev + prev2, prev)

  n match
    case 0 => 0
    case 1 => 1
    case _ => go(2, 1, 0)

@main def main(n: Int): Unit =
  println(fibonacci(n))
  println(fib(n))
