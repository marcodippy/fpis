package parsing


import ReferenceTypes._

import scala.util.matching.Regex


object ReferenceTypes {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {

    def toEither: Either[ParseError, A] = this match {
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)
    }

    def mapError(f: ParseError => ParseError): Result[A] =
      this match {
        case Failure(e, c) => Failure(f(e), c)
        case _ => this
      }

    def addCommit(isCommitted: Boolean): Result[A] =
      this match {
        case Failure(e, c) => Failure(e, c || isCommitted)
        case _ => this
      }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] =
      this match {
        case Success(a, m) => Success(a, n + m)
        case _ => this
      }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}

object Reference extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)).toEither

  override implicit def string(s: String): Parser[String] =
    loc => {
      val i = firstNonmatchingIndex(loc.input, s, loc.offset)
      if (i == -1) //matched
        Success(s, s.length)
      else
        Failure(loc.advanceBy(i).toError(s"'$s'"), i != 0)
    }

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    loc => s1(loc) match {
      case Failure(_, false) => s2(loc)
      case r => r
    }

  override def slice[A](p: Parser[A]): Parser[String] =
    loc => p(loc) match {
      case Success(_, charsConsumed) => Success(loc.input.substring(loc.offset, loc.offset + charsConsumed), charsConsumed)
      case f@Failure(_, _) => f
    }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    loc => p(loc) match {
      case Success(s, charsConsumed) => f(s)(loc.advanceBy(charsConsumed)).addCommit(charsConsumed != 0).advanceSuccess(charsConsumed)
      case err@Failure(_, _) => err
    }

  override implicit def regex(r: Regex): Parser[String] =
    loc => r.findPrefixOf(loc.input) match {
      case Some(value) => Success(value, value.length)
      case None => Failure(loc.toError("regex " + r), false)
    }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(pErr => pErr.push(loc, msg))

  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  //to avoid stack overflow
  override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      val buf = new collection.mutable.ListBuffer[A]

      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a, n) => buf += a; go(p, offset + n)
          case f@Failure(_, true) => f
          case Failure(_, _) => Success(buf.toList, offset)
        }
      }

      go(p, 0)
    }
}
