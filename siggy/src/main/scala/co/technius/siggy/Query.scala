package co.technius.siggy

case class Query(tparams: List[String], params: List[String])

object Query {
  private object Parser {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace(" ".rep)
    }

    import fastparse.noApi._
    import White._

    val string = P(CharPred(_.isLetter).rep(min = 1))

    val tparams = P("[" ~ string.!.rep(min = 1, sep = ",") ~ "]")
    val params = P(string.!.rep(min = 1, sep = "=>"))
    val query = P(tparams.? ~/ params) map {
      case (tpsOpt, ps) => Query(tpsOpt.map(_.toList).getOrElse(List.empty), ps.toList)
    }
  }

  val parser = Parser.query
  def parse(s: String): Either[String, Query] = parser.parse(s) match {
    case fastparse.all.Parsed.Success(res, _) => Right(res)
    case fastparse.all.Parsed.Failure(_, _, extra) => Left(extra.traced.trace)
  }
}
