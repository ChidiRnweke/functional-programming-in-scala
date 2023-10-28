package Tree
enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(value)         => 1
    case Branch(left, right) => 1 + left.size + right.size

  def depth: Int =
    this match
      case Leaf(value)  => 1
      case Branch(l, r) => 1 + l.depth.max(r.depth)

  def map[B](f: A => B): Tree[B] =
    this match
      case Leaf(a)      => Leaf(f(a))
      case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: (A) => B, g: (B, B) => B): B =
    this match
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

import Tree.*

object Tree:

  extension (t: Tree[Int])
    def firstPositive: Int =
      t match
        case Leaf(i) => i
        case Branch(l, r) =>
          val lpos = l.firstPositive
          if lpos > 0 then lpos else r.firstPositive

    def maximum: Int =
      def runningMax(tree: Tree[Int]): Int =
        tree match
          case Leaf(value)  => value
          case Branch(l, r) => l.maximum.max(r.maximum)

      runningMax(t)
