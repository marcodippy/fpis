package monoids

import datastructures.{Branch, Leaf, Tree}
import state.SimpleRNG


trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = List.empty[A]
  }

  //[10.1] Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }


  //[10.2] Give a Monoid instance for combining Option values.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero: A = m.zero
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  //[10.3] A function having the same argument and return type is sometimes called an endofunction. Write a monoid for endofunctions.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override def zero: A => A = identity
  }

  //[10.5] Implement foldMap.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //[10.6] Hard: The foldMap function can be implemented using either foldLeft or foldRight.
  // But you can also write foldLeft and foldRight using foldMap! Try it.

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val ff: A => (B => B) = f.curried
    val b2b: B => B = foldMap(as, endoMonoid[B])(ff)
    b2b(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val b2b: B => B = foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))
    b2b(z)
  }

  //[10.7] Implement a foldMap for IndexedSeq. Your implementation should use the strategy of splitting the sequence in two,
  // recursively processing each half, and then adding the answers together with the monoid.
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty)
      m.zero
    else if (v.length == 1)
      f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  //[10.8] Also implement a parallel version of foldMap using the library we developed in chapter 7.
  // Hint: Implement par, a combinator to promote Monoid[A] to a Monoid [Par[A]], and then use this to implement parFoldMap.
  object ParFoldMap {

    import parallelism.NonBlocking._
    import parallelism.NonBlocking.Par.toParOps

    def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = {
        Par.map2(a1, a2)(m.op)
      }

      override def zero: Par[A] = Par.unit(m.zero)
    }

    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
      Par.parMap(v)(f).flatMap { bs => foldMapV(bs, par(m))(b => Par.lazyUnit(b)) }
  }

  //[10.9] Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need to come up with a creative Monoid.
  def isOrdered(as: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a: Option[(Int, Int, Boolean)], b: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a, b) match {
        case (Some((a1, a2, x)), Some((b1, b2, y))) => Some((a1, b2, x && y && a2 <= b1))
        case (aa, None) => aa
        case (None, bb) => bb
      }

      val zero: None.type = None
    }
    foldMapV(as, m)(i => Some((i, i, true))).forall(_._3)
  }

  //[10.16] if types A and B are monoids, then the tuple type (A, B) is also a monoid (called their product).
  // Prove it. Notice that your implementation of op is obviously associative so long as A.op and B.op are both associative.
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))

    override def zero: (A, B) = (ma.zero, mb.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero: Map[K, V] = Map[K, V]()

    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      }
  }

  //[10.17] Write a monoid instance for functions whose results are monoids.
  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(f: A => B, g: A => B): A => B = a => mb.op(f(a), g(a))

    override def zero: A => B = _ => mb.zero
  }

  //[10.18] A bag is like a set, except that it’s represented by a map that contains one entry
  //per element with that element as the key, and the value under that key is the number of times
  //the element appears in the bag. Use monoids to compute a “bag” from an IndexedSeq.
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map[A, Int](a -> 1))

}

object MonoidLaws {

  import testing._
  import testing.Prop._

  //[10.4] Use the property-based testing framework we developed in part 2 to implement a property for the monoid laws.
  // Use your property to test the monoids we’ve written.
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val triples: Gen[(A, A, A)] = for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)

    forAll(triples) { case (x, y, z) => m.op(x, m.op(y, z)) == m.op(m.op(x, y), z) } &&
      forAll(gen)(x => m.op(x, m.zero) == x && m.op(m.zero, x) == x)
  }

  def homomorphism[A, B](m1: Monoid[A], m2: Monoid[B], genA: SGen[A])(f: A => B): Prop =
    forAll(genA ** genA) { case (a1, a2) => m2.op(f(a1), f(a2)) == f(m1.op(a1, a2)) }

  def isomorphism[A, B](m1: Monoid[A], m2: Monoid[B], genA: SGen[A], genB: SGen[B])(f: A => B)(g: B => A): Prop =
    homomorphism(m1, m2, genA)(f) && homomorphism(m2, m1, genB)(g) &&
      forAll(genA ** genB) { case (a, b) => (f andThen g) (a) == a && (g andThen f) (b) == b }
}

object WordCount {

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //[10.10] Write a monoid instance for WC and make sure that it meets the monoid laws.
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(l, cnt, r)) => Part(s1 + l, cnt, r)
      case (Part(l, cnt, r), Stub(s1)) => Part(l, cnt, r + s1)
      case (Part(l1, cnt1, r1), Part(l2, cnt2, r2)) => Part(l1, cnt1 + (if ((r1 + l2).isEmpty) 0 else 1) + cnt2, r2)

    }

    override def zero: WC = Stub("")
  }

  //[10.11] Use the WC monoid to implement a function that counts words in a String
  //by recurively splitting it into substrings and counting the words in those substrings.
  def countWords(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    def unstub(s: String): Int = if (s.isEmpty) 0 else 1

    Monoid.foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(chars) => unstub(chars)
      case Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)
    }
  }
}

object LawsTest extends App {

  import Monoid._
  import MonoidLaws._
  import testing._

  val p = homomorphism(stringMonoid, intAddition, Gen.string)(_.length)
  println(p.run(100, 100, SimpleRNG(System.currentTimeMillis)))

  val pp = isomorphism(stringMonoid, listMonoid[Char], Gen.string, Gen.string.map(_.toList))(_.toList)(_.mkString)
  println(pp.run(100, 100, SimpleRNG(System.currentTimeMillis)))

  val pp2 = isomorphism(booleanOr, booleanAnd, Gen.boolean.unsized, Gen.boolean.unsized)(b => !b)(b => !b)
  println(pp2.run(100, 100, SimpleRNG(System.currentTimeMillis)))
}


trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  //[10.15] Any Foldable structure can be turned into a List. Write this conversion in a generic way:
  def toList[A](fa: F[A]): List[A] =
    foldMap(fa)(a => List(a))(listMonoid) //or foldRight(fa)(List.empty[A])(_ :: _)
}

object Foldable {
  //[10.12] Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
  //Remember that foldRight, foldLeft, and foldMap can all be implemented in terms of each other,
  //but that might not be the most efficient implementation.

  val foldableList: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  val foldableIndexedSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = Monoid.foldMapV(as, mb)(f)
  }

  val foldableStream: Foldable[Stream] = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  //[10.13] Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it.
  val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  }

  //[10.14] Write a Foldable[Option] instance.
  val foldableOption: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case Some(a) => f(a, z)
      case None => z
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case Some(a) => f(z, a)
      case None => z
    }

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Some(a) => f(a)
      case None => mb.zero
    }
  }

}








