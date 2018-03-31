package parallelism

import java.util.concurrent._


object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  //[7.4] Using lazyUnit, write a function to convert any function A => B to one that evaluates its result asynchronously.
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)


  //[7.5] Write a function to convert a List[Par[B]] to the Par[List[B]], called sequence. No additional primitives are required. Do not call run.
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((pa, acc) => map2(pa, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //[7.6] Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val lstOfFilters = as.map(asyncF { a => if (f(a)) List(a) else List.empty[A] })
    map(sequence(lstOfFilters))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  //[7.11] Implement choiceN and then choice in terms of choiceN.
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => run(es)(choices(run(es)(n).get()))

  def choiceWithChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(bool => if (bool) 0 else 1))(List(t, f))

  //[7.12] implement choiceMap
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => run(es)(choices(run(es)(key).get))

  //[7.13] Implement this new primitive chooser, and then use it to implement choice and choiceN.
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => run(es)(choices(run(es)(pa).get))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => run(es)(f(run(es)(a).get))

  //[7.14] implement join
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinWithFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def flatMapWithJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))
}
