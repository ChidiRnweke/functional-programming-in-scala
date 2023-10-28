import java.util.concurrent.TimeUnit

class ExecutorService:
  def submit[A](a: Callable[A]): Future[A]

trait Callable[A]:
  def call: A

trait Future[A]:
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    val fbs = ps.map(a => lazyUnit(f(a)))
    sequence(fbs)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      val pars =
        as.map(asyncF(a => if f(a) then List(a) else Nil))
      sequence(pars).map(_.flatten)

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2(acc)(_ :: _))

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] { def call = a(es).get })

  def lazyUnit[A](a: => A): Par[A] = fork(es => UnitFuture(a))

  def sortPar(parList: Par[List[Int]]) =
    parList.map(_.sorted)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        new Future[C]:
          private val futureA = pa(es)
          private val futureB = pb(es)
          @volatile private var cache: Option[C] = None
          def isDone = cache.isDefined
          def get = get(Long.MaxValue, TimeUnit.NANOSECONDS)

          def get(timeout: Long, units: TimeUnit) =
            val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, units)
            val started = System.nanoTime
            val a = futureA.get(timeoutNs, TimeUnit.NANOSECONDS)
            val elapsed = System.nanoTime - started
            val b = futureB.get(timeoutNs - elapsed, TimeUnit.NANOSECONDS)
            val c = f(a, b)
            cache = Some(c)
            c
          def isCancelled = futureA.isCancelled || futureB.isCancelled
          def cancel(evenIfRunning: Boolean) =
            futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)
