package chapter8
import State.*
import RNG.*
import Prop.*
import Gen.*

opaque type Gen[+A] = State[RNG, A]

def listOf[A](a: Gen[A]): Gen[List[A]] = ???

def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

object Gen:
  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def boolean: Gen[Boolean] =
    State(RNG.double).map(_ < 0)

  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

object Prop:
  opaque type FailedCase = String
  opaque type SuccessCount = Int

trait Prop:
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
