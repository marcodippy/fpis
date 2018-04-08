package streamingio

import iomonad.Monad

object SimpleProcess {

  sealed trait Process[I, O] {
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }

      go(this)
    }

    //[15.5] Implement |> as a method on Process. Let the types guide your implementation.
    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
      case Halt() => Halt()
      case Emit(head, tail) => Emit(head, this |> tail)
      case Await(recvO) => this match {
        case Halt() => Halt() |> recvO(None)
        case Emit(head, tail) => tail |> recvO(Some(head))
        case Await(recvI) => Await(i => recvI(i) |> p2)
      }
    }

    def map[O2](f: O => O2): Process[I, O2] = this |> Process.lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }


    //[15.6] Implement zipWithIndex. It emits a running count of values emitted along with each value;
    // for example, Process("a", "b").zipWithIndex yields Process(("a", 0), ("b", 1)).
    def zipWithIndex: Process[I, (O, Int)] =
      this.zip(Process.count[I].map(cnt => cnt - 1))

    def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] =
      Process.zip(this, p)

    def orElse(p: Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Await(recv) => Await {
        case None => p
        case x => recv(x)
      }
      case _ => this
    }

  }

  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]

  object Process {
    def liftOne[I, O](f: I => O): Process[I, O] =
      Await {
        case Some(i) => Emit(f(i))
        case None => Halt()
      }

    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    def filter[I](p: I => Boolean): Process[I, I] =
      Await[I, I] {
        case Some(i) if p(i) => Emit(i)
        case _ => Halt()
      }.repeat


    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] =
        await[Double, Double](i => Emit(i + acc, go(i + acc)))

      go(0.0)
    }

    //[15.1] Implement take, which halts the Process after it encounters the given number of elements,
    // and drop, which ignores the given number of arguments and then emits the rest.
    // Also implement takeWhile and dropWhile, that take and drop elements as long as the given predicate remains true.

    def await[I, O](f: I => Process[I, O], ifNone: => Process[I, O] = Halt[I, O]()): Process[I, O] = Await[I, O] {
      case Some(i) => f(i)
      case _ => ifNone
    }

    def id[I]: Process[I, I] = lift(identity)

    def take[I](n: Int): Process[I, I] =
      if (n <= 0) Halt()
      else await[I, I](i => Emit(i, take(n - 1)))

    def drop[I](n: Int): Process[I, I] =
      if (n <= 0) id[I]
      else await[I, I](_ => drop[I](n - 1))


    def takeWhile[I](f: I => Boolean): Process[I, I] =
      await[I, I](i => if (f(i)) Emit(i) else Halt())

    def dropWhile[I](f: I => Boolean): Process[I, I] =
      await[I, I](i => if (f(i)) dropWhile(f) else Emit(i, id))

    //[15.2] Implement count. It should emit the number of elements seen so far.
    // For instance, count(Stream("a", "b", "c")) should yield Stream(1, 2, 3) (or Stream(0, 1, 2, 3), your choice).
    def count[I]: Process[I, Int] = {
      def go(acc: Int): Process[I, Int] =
        await[I, Int](_ => Emit(acc + 1, go(acc + 1)))

      go(0)
    }

    //[15.3] Implement mean. It should emit a running average of the values seen so far.
    def mean: Process[Double, Double] = {
      def go(sum: Double, cnt: Double): Process[Double, Double] =
        await[Double, Double](i => Emit((sum + i) / (cnt + 1), go(sum + i, cnt + 1)))

      go(0, 0)
    }

    def emit[I, O](o: O, p: Process[I, O] = Halt[I, O]()): Emit[I, O] = Emit(o, p)

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await[I, O](i => f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      })

    //[15.4] Write sum and count in terms of loop.
    def sumWithLoop: Process[Double, Double] =
      loop[Double, Double, Double](0)((n, sum) => (sum + n, sum + n))

    def countWithLoop[I]: Process[I, Int] =
      loop[Int, I, Int](0)((i, cnt) => (cnt + 1, cnt + 1))


    def monad[I]: Monad[Process[I, ?]] =
      new Monad[Process[I, ?]] {
        def unit[O](o: => O): Process[I, O] = Emit(o)

        def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] =
          p flatMap f
      }

    //[15.7] ome up with a generic combinator that lets you express mean in terms of sum and count.
    // Define this combinator and implement mean in terms of it.
    def zip[I, O, O2](p1: Process[I, O], p2: Process[I, O2]): Process[I, (O, O2)] =
      (p1, p2) match {
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        case (Emit(h1, t1), Emit(h2, t2)) => Emit((h1, h2), zip(t1, t2))
        case (Await(recv1), _) => Await(i => zip(recv1(i), feed(i)(p2)))
        case (_, Await(recv2)) => Await(i => zip(feed(i)(p1), recv2(i)))
      }

    def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] =
      p match {
        case Halt() => p
        case Emit(h, t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)
      }

    def meanWithZip: Process[Double, Double] =
      count.zip(sum) |> lift { case (i, d) => i / d }


    //[15.8] Implement exists. There are multiple ways to implement it, given that exists(_ % 2 == 0)(Stream(1,3,5,6,7))
    // could produce Stream(true) (halting, and only yielding the final result),
    // Stream(false,false,false,true) (halting, and yielding all intermediate results),
    // or Stream(false,false,false,true,true) (not halting, and yielding all the intermediate results).
    // Note that because |> fuses, there’s no penalty to implementing the “trimming” of this last form with a separate combinator.
    def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> any

    def any: Process[Boolean, Boolean] =
      loop(false)((bool, s) => (s || bool, s || bool))

    def existsResult[I](f: I => Boolean): Process[I, Boolean] =
      exists(f) |> takeThrough(!_) |> dropWhile(!_) |> echo.orElse(emit(false))

    def takeThrough[I](f: I => Boolean): Process[I, I] =
      takeWhile(f) ++ echo

    def echo[I]: Process[I, I] = await(i => emit(i))

  }

  object Ex15_9 {
    //[15.9] Write a program that reads degrees Fahrenheit as Double values from a file, one value per line,
    // sends each value through a process to convert it to degrees Fahrenheit, and writes the result to another file.
    // Your program should ignore blank lines in the input file, as well as lines that start with the # character.
    // You can use the function toCelsius.
    def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

    import Process._

    def convertFahrenheit: Process[String, String] =
      filter((line: String) => line.nonEmpty) |>
        filter((line: String) => !line.startsWith("#")) |>
        lift(line => toCelsius(line.toDouble).toString)
  }

}

import streamingio.Process._

trait Process[F[_], O] {
  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(req, recv) =>
      Await(req, recv andThen (_ flatMap f))
  }
}

object Process {

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Exception

  case object Kill extends Exception

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(req, recv)

}