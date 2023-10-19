def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
  def safeGet(idx: Int): Option[A] =
    if (idx < as.length) Some(as(idx)) else None

  def inner(idx: Int): Boolean = safeGet(idx + 1) match
    case Some(nextVal) if gt(nextVal, as(idx)) => inner(idx + 1)
    case Some(_)                               => false
    case None                                  => true
  inner(0)

@main def sorting: Unit =
  println(isSorted(Array(1, 2, 3), _ > _))
  println(isSorted(Array(1, 2, 1), _ > _))
  println(isSorted(Array(3, 2, 1), _ < _))
  println(isSorted(Array(1, 2, 3), _ < _))
