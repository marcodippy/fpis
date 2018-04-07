package applicative

import monads.{Functor, Monad}
import monoids.{Foldable, Monoid}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  //[12.1] Transplant the implementations of as many combinators as you can from Monad to Applicative,
  // using only map2 and unit, or methods implemented in terms of them.
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  //[12.2] the name applicative comes from the fact that we can formulate the Applicative interface using
  // an alternate set of primitives, unit and the function apply, rather than unit and map2.
  // Show that this formulation is equivalent in expressiveness by defining map2 and map in terms of unit and apply.
  // Also establish that apply can be implemented in terms of map2 and unit.

  def mapWithApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2WithApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))


  //[12.3] The apply method is useful for implementing map3, map4, and so on, and the pattern is straightforward.
  // Implement map3 and map4 using only unit, apply, and the curried method available on functions

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(
      apply(
        apply(unit(f.curried))(fa)
      )(fb)
    )(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(
      apply(
        apply(
          apply(unit(f.curried))(fa)
        )(fb)
      )(fc)
    )(fd)


  //[12.8] Just like we can take the product of two monoids A and B to give the monoid (A, B),
  // we can take the product of two applicative functors. Implement this function:
  //  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f]
  def product[G[_]](G: Applicative[G]): Applicative[Lambda[x => (F[x], G[x])]] = {
    val self = this
    new Applicative[Lambda[x => (F[x], G[x])]] {
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    }
  }

  //[12.9] Hard: Applicative functors also compose another way!
  // If F[_] and G[_] are applicative functors, then so is F[G[_]].Implement this function:
  // def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f]
  def compose[G[_]](G: Applicative[G]): Applicative[Lambda[x => F[G[x]]]] = {
    val self = this
    new Applicative[Lambda[x => F[G[x]]]] {
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
    }
  }

  //[12.12] On the Applicative trait, implement sequence over a Map rather than a List:
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fv), fkvs) => map2(fv, fkvs)((v, kvs) => kvs + (k -> v)) }
}

object Applicative {

  //[12.6] Write an Applicative instance for Validation that accumulates errors in Failure.
  // Note that in the case of Failure there’s always at least one error, stored in head.
  // The rest of the errors accumulate in the tail.
  def validationApplicative[E]: Applicative[Validation[E, ?]] = new Applicative[Validation[E, ?]] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++ Vector(hb) ++ tb)
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[Const[M, ?]] =
    new Applicative[Const[M, ?]] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  //[12.14] Implement map in terms of traverse as a method on Traverse[F].
  // This establishes that Traverse is an extension of Functor and that the traverse function
  // is a generalization of map (for this reason we sometimes call these traversable functors).
  // Note that in implementing map, you can call traverse with your choice of Applicative[G].
  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[Const[M, ?], A, Nothing](as)(f)(monoidApplicative(mb))

  import state.State

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, ?], A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) =>
      for {
        i <- State.get[Int]
        _ <- State.set(i + 1)
      } yield (a, i)
    ).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) =>
      for {
        as <- State.get[List[A]] // Get the current state, the accumulated list.
        _ <- State.set(a :: as) // Add the current element and set the new list as the new state.
      } yield ()
    ).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  //[12.16] There’s an interesting consequence of being able to turn any traversable functor into a reversed list:
  //we can write, once and for all, a function to reverse any traversable functor! Write this function
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  //[12.17] Use mapAccum to give a default implementation of foldLeft for the Traverse trait.
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  //[12.18] Use applicative functor products to write the fusion of two traversals.
  // This function will, given two functions f and g, traverse fa a single time, collecting the results of both functions at once.
  def fuse[G[_], H[_], A, B](fa: F[A])
                            (f: A => G[B], g: A => H[B])
                            (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[Lambda[x => (G[x], H[x])], A, B](fa)(a => (f(a), g(a)))(G product H)


  //[12.19] Implement the composition of two Traverse instances.
  def compose[G[_]](implicit G: Traverse[G]): Traverse[Lambda[x => F[G[x]]]] =
    new Traverse[Lambda[x => F[G[x]]]] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }
}

object Traverse {
  //[12.13] Write Traverse instances for List, Option, and Tree.
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B]))((a, gbs) => G.map2(f(a), gbs)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case None => G.unit(None)
        case Some(a) => G.map(f(a))(Some(_))
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

}