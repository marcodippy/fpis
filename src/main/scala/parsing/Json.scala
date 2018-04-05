package parsing



trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[Err, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def jval: Parser[JSON] = obj | arr | literal

    def obj: Parser[JObject] = surround("{", "}")(keyval.sep(",").map(keyvals => JObject(keyvals.toMap)))

    def keyval: Parser[(String, JSON)] = escapedQuoted ** (":" *> jval)

    def arr: Parser[JArray] = surround("[", "]")(jval.sep(",").map(jval => JArray(jval.toIndexedSeq)))

    def bool: Parser[JBool] = "true".as(JBool(true)) | "false".as(JBool(false))
    def num: Parser[JNumber] = double.map(JNumber)
    def nil: Parser[JNull.type] = "null".as(JNull)
    def str: Parser[JString] = escapedQuoted.map(JString)

    def literal: Parser[JSON] = bool | num | str | nil

    root(whitespace *> (obj | arr))
  }
}
