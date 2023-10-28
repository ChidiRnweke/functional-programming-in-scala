def findFirst(array: Seq[String], key: String): Option[Int] =
  def loop(seq: Seq[String], acc: Int): Option[Int] =
    seq.headOption match
      case None                        => None
      case Some(value) if value == key => Some(acc)
      case _                           => loop(seq.tail, acc + 1)
  loop(array, 0)

def findFirstPoly[A](array: Seq[A], key: A): Option[Int] =
  def loop[A](seq: Seq[A], acc: Int): Option[Int] =
    seq.headOption match
      case None                        => None
      case Some(value) if value == key => Some(acc)
      case _                           => loop(seq.tail, acc + 1)
  loop(array, 0)

def findFirstSuperPoly[A](array: Seq[A], p: A => Boolean): Option[Int] =
  def loop(seq: Seq[A], n: Int): Option[Int] =
    seq.headOption match
      case None                            => None
      case Some(value) if p(value) == true => Some(n)
      case _                               => loop(seq.tail, n + 1)
  loop(array, 0)

@main def first: Unit =
  println(findFirstSuperPoly(Array(7, 9, 13), (x: Int) => x == 9))
