package LazyList
import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] =
    this match
      case Empty      => None
      case Cons(h, _) => Some(h())

  def toList: List[A] =
    this match
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _                    => empty

  final def drop(n: Int): LazyList[A] =
    this match
      case _ if (n <= 0) => this
      case Empty         => Empty
      case Cons(_, t)    => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] =
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => empty

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) | b)

  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _          => acc

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile_fr(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  def headOption_fr: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else acc)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, acc) => f(a).append(acc))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def unfold_map[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def unfold_take(n: Int): LazyList[A] =
    unfold((n, this)) {
      case (0, _)            => None
      case (s, Cons(h1, t1)) => Some(h1(), (s - 1, t1()))
    }

  def takeWhile_unfold(p: A => Boolean): LazyList[A] =
    unfold(this) {
      case Empty                => None
      case Cons(h, t) if p(h()) => Some((h(), t().takeWhile_unfold(p)))
      case _                    => None
    }

  def zipWith[B, C](that: LazyList[B], f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Empty, _) | (_, Empty)      => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2())), (t1(), t2()))
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty)        => None
      case (Empty, Cons(h3, t3)) => Some((None, Some(h3())), (Empty, t3()))
      case (Cons(h4, t4), Empty) => Some((Some(h4()), None), (t4(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
    }

  def startsWith[A](prefix: LazyList[A]): Boolean =
    zipWith(prefix, _ == _).forAll(_ == true)

  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s.drop(1)))
    }.append(LazyList(empty))

  def hasSubsequence[A](l: LazyList[A]): Boolean = tails.exists(_.startsWith(l))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)): (a, b0) => // -> is just a tuple / pair
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    .apply(1)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  def fibs(n: Int): LazyList[Int] =
    def loop(a: Int, b: Int): LazyList[Int] =
      LazyList.cons(a, loop(b, a + b))

    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty

  def unfold_continually[A](a: A): LazyList[A] =
    unfold(a)((s: A) => Some((s, s)))

  def unfold_from[A](a: Int): LazyList[Int] =
    unfold(a)((s: Int) => Some((s, s + 1)))

  def unfold_fibs[A](n: Int): LazyList[Int] =
    unfold((0, 1))((a, b) => Some((a, (b, a + b))))

@main def foo: Unit =
  println(LazyList(1, 2, 3).tails.map(_.toList).toList)
