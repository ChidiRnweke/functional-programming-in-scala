package Either

import scala.util.control.NonFatal
enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] =
    this match
      case Right(a) => Right(f(a))
      case Left(e)  => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Right(value) => f(value)
      case Left(value)  => Left(value)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Left(_)      => b
      case Right(value) => Right(value)

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      aa <- this
      bb <- that
    yield f(aa, bb)

  def map2_[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => that.map(b => f(a, b)))

object Either:
  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, acc) =>
      f(a).map2_(acc)(_ :: _)
    )

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(as => as)

import Either.*

def mean(xs: Seq[Double]): Either[String, Double] =
  if xs.isEmpty then Left("mean of empty list!")
  else Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
  catchNonFatal(x / y)
