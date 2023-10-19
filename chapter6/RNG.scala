package RNG

trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

def nonNegativeInt(rng: RNG): (Int, RNG) =
  rng.nextInt match
    case (i, newRNG) if i >= 0            => (i, newRNG)
    case (i, newRNG) if i == Int.MinValue => nonNegativeInt(newRNG)
    case (i, newRNG)                      => (-i, newRNG)

def double(rng: RNG): (Double, RNG) =
  val (i, newRNG) = nonNegativeInt(rng)
  (i / (Int.MaxValue.toDouble + 1), newRNG)

def intDouble(rng: RNG): ((Int, Double), RNG) =
  val (int, rng2) = rng.nextInt
  val (dub, rng3) = double(rng2)
  ((int, dub), rng3)

def doubleInt(rng: RNG): ((Double, Int), RNG) =
  val ((int, dub), rng2) = intDouble(rng)
  ((dub, int), rng2)

def double3(rng: RNG): ((Double, Double, Double), RNG) =
  val (dub1, rng2) = double(rng)
  val (dub2, rng3) = double(rng2)
  val (dub3, rng4) = double(rng3)

  ((dub1, dub2, dub3), rng3)

def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  val intsAndRNGs = List.unfold((rng, count)) {
    case (r, c) if c > 0 =>
      val (i, nextRNG) = r.nextInt
      Some((i, nextRNG), (nextRNG, c - 1))
    case _ => None
  }
  (
    intsAndRNGs.map((i, _) => i),
    intsAndRNGs.lastOption.map(_._2).getOrElse(rng)
  )
