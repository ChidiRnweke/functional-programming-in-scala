import RNG.*

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = rng => rng.nextInt

def unit[A](a: A): Rand[A] = rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng =>
    val (a, newRNg) = s(rng)
    (f(a), newRNg)

def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - (i % 2))

def double: Rand[Double] =
  map(nonNegativeInt)(i => i / Int.MaxValue)

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng =>
    val (ra2, rng1) = ra(rng)
    val (rb2, rng2) = rb(rng)
    (f(ra2, rb2), rng2)

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

val randIntDouble: Rand[(Int, Double)] = both(int, double)
val randDoubleInt: Rand[(Double, Int)] = both(double, int)

def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
  rs.foldRight[Rand[List[A]]](rng => (List.empty[A], rng)) { (ra, acc) =>
    map2(ra, acc)(_ :: _)
  }

def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
  rng =>
    val (a, r1) = r(rng)
    val rb = f(a)
    val rb2 = rb(r1)
    rb2

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt) { i =>
    if (i < n) unit(i)
    else nonNegativeLessThan(n)
  }

def flatMapMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => (rng => (f(a), rng)))

def flatMapMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra)(a => map(rb)(b => f(a, b)))

@main def rand_main(): Unit = {
  val rng = SimpleRNG(1)
  val u = unit(100)(rng)
  println(u)
}
