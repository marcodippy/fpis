package testing

/*

import state.RNG

private object Exercise8_3 {

  //[8.3] Assuming the following representation of Prop, implement && as a method of Prop.
  trait Prop {
    def check(): Boolean

    def &&(p: Prop): Prop = new Prop {
      override def check(): Boolean = Prop.this.check() && p.check()
    }
  }

}

object Exercise8_9 {

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int
    type Result = Option[(FailedCase, SuccessCount)]
  }

  //[8.9] implement && and || for composing Prop values.
  //Notice that in the case of failure we don’t know which property was responsible, the left or the right.
  // Can you devise a way of handling this, perhaps by allowing Prop values to be assigned a tag
  // or label which gets displayed in the event of a failure?
  case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {
    def &&(p2: Prop) = Prop { (testCases, rng) =>
      val r1 = run(testCases, rng)
      r1 flatMap { _ => p2.run(testCases, rng) }
    }

    def ||(p2: Prop) = Prop { (testCases, rng) =>
      val r1 = run(testCases, rng)
      r1 orElse p2.run(testCases, rng)
    }
  }

}

private object PropV1 {

  import PropV1.Prop.{Result, TestCases}
  import laziness.Stream

  case class Prop(run: (TestCases, RNG) => Result)

  object Prop {
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

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = new Prop(
      (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
    )

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }

}
*/