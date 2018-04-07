package iomonad

object IoV0 {

  sealed trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      def run = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      def run: B = f(self.run).run
    }
  }


  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run: A = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)
  }

}


object IoV1 {

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends IO[A]

  case class Suspend[A](resume: () => A) extends IO[A]

  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]


  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)

    def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] =
      a flatMap f

    def suspend[A](a: => IO[A]): IO[A] =
      Suspend(() => ()).flatMap { _ => a }
  }

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

}

object IoV2 {

  import parallelism.NonBlocking._

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)

    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A]

  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] =
    async match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => async
    }

  def run[A](async: Async[A]): Par[A] =
    step(async) match {
      case Return(a) => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match {
        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
        case _ => sys.error("Impossible, since `step` eliminates these cases")
      }
    }
}

object IoV3 {


  //[13.1] Free is a monad for any choice of F. Implement map and flatMap methods on the Free trait,
  // and give the Monad instance for Free[F,_]
  sealed trait Free[F[_], A] {
    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))

    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]


  def freeMonad[F[_]]: Monad[Free[F, ?]] = new Monad[Free[F, ?]] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
  }

  //[13.2] Implement a specialized tail-recursive interpreter, runTrampoline, for running a Free[Function0,A].
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(s, f) => s match {
      case Return(aa) => runTrampoline {
        f(aa)
      }
      case Suspend(ss) => runTrampoline {
        f(ss())
      }
      case FlatMap(ss, ff) => runTrampoline {
        ss.flatMap(a => ff(a) flatMap f)
      }
    }
  }

  //[13.3] Hard: Implement a generic interpreter for Free[F, A], given a Monad[F].
  @annotation.tailrec
  def step[F[_], A](fa: Free[F, A]): Free[F, A] =
    fa match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => fa
    }

  def run[F[_], A](fa: Free[F, A])(implicit F: Monad[F]): F[A] =
    step(fa) match {
      case Return(a) => F.unit(a)
      case Suspend(s) => s
      case FlatMap(Suspend(s), f) => F.flatMap(s)(a => run(f(a)))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }


  import parallelism.NonBlocking._

  sealed trait Console[A] {
    def toPar: Par[A]

    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)

    def toThunk = () => run

    import scala.io.StdIn._
    def run: Option[String] =
      try Some(readLine)
      catch {
        case _: Exception => None
      }

  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))

    def toThunk = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  import language.higherKinds

  //[13.4]  It turns out that runConsoleFunction0 isn’t stack-safe, since flatMap isn’t stack-safe for Function0.
  // Implement translate using runFree, and then use it to implement runConsole in a stack-safe way.
  /*
  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend {
        fg(a)
      }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline {
      translate(a)(new (Console ~> Function0) {
        def apply[A](c: Console[A]) = c.toThunk
      })
    }
 */


}