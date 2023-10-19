package Option

enum Option[+A]:
  case Some(get: A)
  case None

  // constraint: only pattern match for map and getOrElse
  def map[B](f: A => B): Option[B] =
    this match
      case Some(get) => Some(f(get))
      case None      => None

  def getOrElse[B >: A](default: => B): B =
    this match
      case Some(get) => get
      case None      => default

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(_ => this).getOrElse(ob)

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
import Option.*

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

object Option:
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, acc) =>
      map2(f(a), acc)(_ :: _)
    )

  def sequenceByTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(av => b.map(bv => f(av, bv)))

def map2_comprehension[A, B, C](a: Option[A], b: Option[B])(
    f: (A, B) => C
): Option[C] =
  for
    aa <- a
    bb <- b
  yield f(aa, bb)

@main def main(): Unit =
  val absO: Option[Double] => Option[Double] = lift(math.abs)

  val ex1 = absO(Some(-1.0))
  println(ex1)
