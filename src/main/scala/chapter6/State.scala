package State

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => s => (f(a), s))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

    def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- other
      yield f(a, b)

  def apply[S, A](f: S => (A, S)): State[S, A] = f
  def unit[S, A](a: A): State[S, A] = s => (a, s)
  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight[State[S, List[A]]](s => (List.empty[A], s)) { (ra, acc) =>
      acc.map2(ra)((as, a) => a :: as)
    }

  def set[S](s: S): State[S, Unit] = _ => ((), s)
  def get[S]: State[S, S] = s => (s, s)
  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()
