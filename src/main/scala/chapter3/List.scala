package fpinscala.datastructures

import scala.annotation.tailrec

enum List[+A]: // covariant type: subtypes are allowed
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int =
    ints match
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double =
    doubles match
      case Nil          => 1.0
      case Cons(0.0, _) => 0
      case Cons(x, xs)  => x * product(xs)

  def tail[A](xs: List[A]): Option[List[A]] =
    xs match
      case Nil           => None
      case Cons(_, tail) => Some(tail)

  def setHead[A](x: A, xs: List[A]): Option[List[A]] =
    xs match
      case Nil           => None
      case Cons(_, tail) => Some(Cons(x, tail))

  def drop[A](as: List[A], n: Int): List[A] =
    as match
      case _ if n <= 0   => as
      case Nil           => Nil
      case Cons(_, tail) => drop(tail, n - 1)

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil                         => Nil
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _                           => as

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def append[A](as: List[A], a: A): List[A] =
    as match
      case Nil        => as
      case Cons(h, t) => Cons(h, append(t, a))

  def init[A](as: List[A]): List[A] =
    as match
      case Nil | Cons(_, Nil) => Nil
      case Cons(h, t)         => Cons(h, init(t))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil        => acc
      case Cons(h, t) => f(h, foldRight(t, acc, f))

  def sumViaFoldRight(ints: List[Int], acc: Int): Int =
    foldRight(ints, 0, _ + _)

  def productViaSumRight(doubles: List[Double], acc: Double): Double =
    foldRight(doubles, 1.0, _ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)

  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil        => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def lengthViaFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0, (acc, _) => acc + 1)

  def sumViaFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0, _ + _)

  def productViaFoldLeft(ints: List[Int]): Double =
    foldLeft(ints, 1, _ * _)

  def reverseViaFoldLeft[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverseViaFoldLeft(as), acc, (b, a) => f(a, b))

  def appendRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))

  def appendLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverseViaFoldLeft(a1), a2, (b, a) => Cons(a, b))

  def flatten[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A], appendRight)

  def addOne(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int], (a, acc) => Cons(a + 1, acc))

  def doubleToString(ints: List[Double]): List[String] =
    foldRight(ints, Nil: List[String], (a, acc) => Cons(a.toString(), acc))

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => appendRight(f(a), acc))

  def filter2[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def elementWiseSum(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match
      case (Nil, _) | (_, Nil)          => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, elementWiseSum(t1, t2))

  def zipMap[A, B, C](xs: List[A], ys: List[B], f: (A, B) => C): List[C] =
    def go(xs: List[A], ys: List[B], acc: List[C]): List[C] =
      (xs, ys) match
        case (Nil, _) | (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) =>
          go(t1, t2, Cons(f(h1, h2), acc))
    go(xs, ys, Nil)

  def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean =
    (xs, sub) match
      case (Nil, _)                                 => false
      case (_, Nil)                                 => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => hasSubsequence(t1, t2)
      case (Cons(h1, t1), Cons(h2, t2)) if h1 != h2 => hasSubsequence(t1, sub)

import List.*

def main(args: Array[String]): Unit = {
  println(hasSubsequence(List(1, 2, 3, 2, 3, 4), List(2, 3, 2)))
}
