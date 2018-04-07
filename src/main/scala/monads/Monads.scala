package monads

import applicative.Applicative

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  //aka "unzip"
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }

}

object Functor {
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  //[11.3] implement sequence and traverse
  override def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((fa, fas) => map2(fa, fas)(_ :: _))

  override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  //[11.4] Implement replicateM
  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  //[11.6]  Implement the function filterM. It’s a bit like filter, except that instead of a function from A => Boolean,
  // we have an A => F[Boolean].
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List.empty[A]))((a, fas) =>
      map2(f(a), fas)((bool, as) => if (bool) a :: as else as)
    )
  }

  //[11.7] Implement the Kleisli composition function compose.
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  //[11.8] Implement flatMap in terms of compose.
  // It seems that we’ve found another minimal set of monad combinators: compose and unit.
  def flatMapWithCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  //[11.12] There’s a third minimal set of monadic combinators: map, unit, and join. Implement join in terms of flatMap.
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  //[11.13] Implement either flatMap or compose in terms of join and map.
  def flatMapWithJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def composeWithJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))


  //[12.20] Implement the composition of two monads where one of them is traversable.
  import applicative.Traverse

  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[Lambda[x => G[H[x]]]] =
    new Monad[Lambda[x => G[H[x]]]] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
    }


}


object Monad {

  import testing._
  import parallelism.NonBlocking._
  import parsing._
  import errorhandling._
  import laziness._
  import datastructures._
  import state._

  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  //[11.1] Write monad instances for Par, Parser, Option, Stream, and List.
  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }

  def parserMonad[Parser[+ _]](p: Parsers[Parser]): Monad[Parser] = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] = p.succeed(a)

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = p.flatMap(fa)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = List.flatMap(fa)(f)
  }

  //[11.2] State looks like it would be a monad too, but it takes two type arguments
  //and you need a type constructor of one argument to implement Monad. Try to implement a State monad
  trait StateMonads[S] {
    type StateS[A] = State[S, A]

    val stateMonad: Monad[StateS] = new Monad[StateS] {
      override def unit[A](a: => A): State[S, A] = State.unit[S, A](a)

      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
    }
  }

  //or using a type lambda (syntax provided by the kind-projector plugin; Monad[State[S, ?]] is equivalent to Monad[({type f[x] = State[S, x]})#f]
  def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
  }


  case class Id[A](a: A) {
    //[11.17] Implement map and flatMap as methods on this class, and give an implementation for Monad[Id].
    def map[B](f: A => B): Id[B] = Id(f(a))

    def flatMap[B](f: A => Id[B]): Id[B] = f(a)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }


  //[11.20] To cement your understanding of monads, give a monad instance for the following type.
  // What are its primitive operations?

  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R]: Monad[Reader[R, ?]] = new Monad[Reader[R, ?]] {
      override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

      override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }

    def ask[R]: Reader[R, R] = Reader(r => r)
  }

  //[12.5]Write a monad instance for Either.
  def eitherMonad[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
  }
}

