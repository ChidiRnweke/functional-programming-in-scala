// Redoing the exercises for practice
package Option2

enum Option2[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option2[B] =
    this match
      case Some(get) => Some(f(get))
      case None      => None

  def flatMap[B](f: A => Option2[B]): Option2[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: B): B =
    this match
      case Some(get) => get
      case None      => default

  def orElse[B >: A](ob: => Option2[B]): Option2[B] =
    this.map(_ => this).getOrElse(ob)

  def filter(f: A => Boolean): Option2[A] =
    flatMap(x => if f(x) then Some(x) else None)

object Option2:
  def lift[A, B](f: A => B): Option2[A] => Option2[B] = _.map(f)

  def map2[A, B, C](a: Option2[A], b: Option2[B])(f: (A, B) => C): Option2[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](as: List[Option2[A]]): Option2[List[A]] =
    as.foldRight[Option2[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => Option2[B]): Option2[List[B]] =
    as.foldRight[Option2[List[B]]](Some(Nil))((a, acc) =>
      map2(f(a), acc)(_ :: _)
    )

import Option2.*

def mean(xs: Seq[Double]): Option2[Double] =
  if xs.isEmpty then None else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option2[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

val absO: Option2[Double] => Option2[Double] =
  lift(math.abs)

val ex1 = absO(Some(-1.0))
