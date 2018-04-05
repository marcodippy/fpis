package parsing

import java.util.regex.Pattern

import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  val numA: Parser[Int] = char('a').many.map(_.size)

  def defaultSucceed[A](a: A): Parser[A] = string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  val numAWithSlice: Parser[Int] = char('a').many.slice.map(_.length)


  //[9.1] Using product, implement the now-familiar combinator map2 and then use this to implement many1 in terms of many.
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  //[9.3] define many in terms of or, map2, and succeed.
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty)

  //[9.4] Using map2 and succeed, implement the listOfN combinator from earlier.
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n == 0) succeed(List.empty[A])
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  //[9.6] Using flatMap and any other combinators, write the context-sensitive parser we couldn’t express earlier.
  //To parse the digits, you can make use of a new primitive, regex, which promotes a regular expression to a Parser.
  // In Scala, a string s can be promoted to a Regex object (which has methods for matching) using s.r,
  // for instance, "[a-zA-Z_][a-zA-Z0-9_]*".r.
  implicit def regex(r: Regex): Parser[String]

  val contextSensitive = for {
    digit <- "[0-9]+".r
    n = digit.toInt
    _ <- listOfN(n, char('a'))
  } yield n

  //[9.7] Implement product and map2 in terms of flatMap.

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      pp1 <- p1
      pp2 <- p2
    } yield (pp1, pp2)

  def map2WithFlatmap[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      pp1 <- p1
      pp2 <- p2
    } yield f(pp1, pp2)

  //[9.8] map is no longer primitive. Express it in terms of flatMap and/or other combinators.
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, b) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]): Parser[A] =
    start *> p <* stop

  def whitespace: Parser[String] = "\\s*".r

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def until(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  def quoted: Parser[String] = string("\"") *> until("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] =
    token(quoted label "string literal")

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p, p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)


  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def attempt: Parser[A] = self.attempt(p)

    def skipL[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def skipR(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

    def opt: Parser[Option[A]] = self.opt(p)

    def surround(start: Parser[Any], stop: Parser[Any]): Parser[A] = self.surround(start, stop)(p)

    def token: Parser[A] = self.token(p)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def sep(separator: Parser[Any]) = self.sep(p, separator)

    def sep1(separator: Parser[Any]) = self.sep1(p, separator)

  }

  object Laws {

    import testing._
    import Prop.forAll

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

  }

}


case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location =
    copy(offset = offset + n)
}

case class ParseError(stack: List[(Location, String)]) {
  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)
}