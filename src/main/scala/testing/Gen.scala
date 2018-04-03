package testing

import java.util.concurrent.{ExecutorService, Executors}

import Prop._
import state._
import laziness.Stream
import parallelism.Par
import parallelism.Par.Par

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        // In case of failure, run the other prop.
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop =
      props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)
         ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }
}


object Gen {
  //[8.4] Implement Gen.choose using this representation of Gen.
  //case class Gen[A](sample: State[RNG, A])
  //It should generate integers in the range start to stopExclusive
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](State[RNG, Int](rng => rng.nextInt).map(num => start + num % (stopExclusive - start)))

  //[8.5] implement unit, boolean, and listOfN

  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))

  def boolean: Gen[Boolean] = Gen[Boolean](State(RNG.boolean))

  def int: Gen[Int] = Gen[Int](State(RNG.nonNegativeInt))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen[List[A]](State.sequence(List.fill(n)(g.sample)))

  //[8.7] Implement union, for combining two generators of the same type into one,
  //by pulling values from each generator with equal likelihood.
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  //[8.8] Implement weighted, a version of union that accepts a weight for each Gen
  //and generates values from each Gen with probability proportional to its weight.
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen[A](State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }

}


case class Gen[+A](sample: State[RNG, A]) {
  //[8.6] Implement flatMap, and then use it to implement this more dynamic version of listOfN.
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen[B](sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN)

  //[8.10] Implement helper functions for converting Gen to SGen.
  def unsized: SGen[A] = SGen[A](_ => this)

  def map[B](f: A => B): Gen[B] = Gen[B](sample.map(f))

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen[C](sample.map2(gb.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))
}


//[8.11] SGen at a minimum supports many of the same operations as Gen, and the implementations are rather mechanical.
//Define some convenience functions on SGen that simply delegate to the corresponding functions on Gen

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen { i => g(i).map(f) }

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen { i => g(i).flatMap(ig => f(ig).g(i)) }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen[(A, B)](n => apply(n) ** s2(n))
}

object SGen {

  //[8.12] Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a Gen.
  //The implementation should generate lists of the requested size.
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen[List[A]](l => g.listOfN(l))

  //[8.13] Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator.
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen[List[A]](l => g.listOfN(l max 1))
}

object Properties {

  val smallInt = Gen.choose(-10, 10)

  val maxProp1 = forAll(SGen.listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  //[8.14] Write a property to verify the behavior of List.sorted, which you can use to sort (among other things) a List[Int].
  //For instance, List(2,1,3).sorted is equal to List(1,2,3).
  val sortedList = forAll(SGen.listOf(smallInt))(l => {
    val sl = l.sorted

    !sl.exists(i => l.contains(i)) && !l.exists(i => sl.contains(i)) && {
      (l.isEmpty && sl.isEmpty) || sl.tail.isEmpty || !sl.zip(sl.tail).exists { case (a, b) => a > b }
    }
  })

  val ES: ExecutorService = Executors.newCachedThreadPool
  val parLaw = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val parLaw2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0, 10) map (Par.unit(_))
  val parLaw3 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  //[8.16] Write a richer generator for Par[Int], which builds more deeply nested parallel computations
  //than the simple ones we gave previously.
  val pint2: Gen[Par[Int]] =
  Gen.choose(-100, 100)
    .listOfN(Gen.choose(0, 20))
    .map(l => l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))

  //[8.17] Express the property about fork from chapter 7, that fork(x) == x.
  val forkLaw = forAllPar(pint2)(n => equal(Par.fork {
    n
  }, n)) tag "fork"
}