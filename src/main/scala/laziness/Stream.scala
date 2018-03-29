package laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case Empty => None
  }

  //[5.1] Write a function to convert a Stream to a List
  //this is not tail recursive
  def toListRecursive: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toListTailRec: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => List.empty[A]
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this, List.empty[A]).reverse
  }

  //to avoid the reverse
  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => {
        buf += h()
        go(t())
      }
    }

    go(this)
  }

  //[5.2] Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  //[5.3] Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsWithFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  //[5.4] Implement forAll, which checks that all elements in the Stream match a given predicate.
  //Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  //[5.5] Use foldRight to implement takeWhile.
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  //[5.6] Hard: Implement headOption using foldRight.
  def headOptionWithFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  //[5.7] Implement map, filter, append, and flatMap using foldRight.
  //The append method should be non-strict in its argument.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)(cons(_, _))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  //[5.13] Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
  //The zipAll function should continue the traversal as long as either stream has more elements
  //it uses Option to indicate whether each stream has been exhausted.
  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), nn) if nn > 1 => Some((h(), (t(), nn - 1)))
      case _ => None
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2)) => Some(((Option.empty[A], Some(h2())), (empty[A], t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), Option.empty[B]), (t1(), empty[B])))
      case _ => None
    }

  //[5.14] Hard: Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
  def startsWith(s: Stream[_]): Boolean =
    this.zipAll(s)
      .takeWhile { case (_, op2) => op2.isDefined }
      .forAll { case (op1, op2) => op1 == op2 }

  //[5.15] Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence,
  //starting with the original Stream.
  //For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append Stream(empty[A])


  def hasSubsequence(s: Stream[_]): Boolean =
    tails exists (_ startsWith s)

  //[5.16] Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate results.
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  //[5.8] Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantMoreEfficient[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  //[5.9] Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //[5.10] Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {
    def go(i1: Int, i2: Int): Stream[Int] =
      cons(i1, go(i2, i1 + i2))

    go(0, 1)
  }

  //[5.11] Write a more general stream-building function called unfold.
  //It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  //[5.12] Write fibs, from, constant, and ones in terms of unfold.
  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def fibsWithUnfold: Stream[Int] =
    unfold((0, 1)) { case (n1, n2) => Some((n1, (n2, n1 + n2))) }

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val onesWithUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

}
