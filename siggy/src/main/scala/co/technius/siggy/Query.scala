package co.technius.siggy

case class Query(tparams: Seq[TypeInfo], params: Seq[TypeInfo])

object Query {
  private object Parser {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace(" ".rep)
    }

    import fastparse.noApi._
    import White._

    val string: P[String] = P(CharPred(c => c.isLetter || c.isDigit || c == '_').rep(min = 1).!)

    val tparams: P[Seq[TypeInfo]] = P("[" ~ tpe.rep(min = 1, sep = ",") ~ "]")
    val tpe: P[TypeInfo] = P(string ~ tparams.?).map {
      case (n, tp) => TypeInfo(n, tp.getOrElse(List.empty))
    }
    val params: P[Seq[TypeInfo]] = P(tpe.rep(min = 1, sep = "=>"))
    val query: P[Query] = P(tparams.? ~/ params) map {
      case (tpsOpt, ps) => Query(tpsOpt.map(_.toList).getOrElse(List.empty), ps.toList)
    }
  }

  val parser = Parser.query
  def parse(s: String): Either[String, Query] = parser.parse(s) match {
    case fastparse.all.Parsed.Success(res, _) => Right(res)
    case fastparse.all.Parsed.Failure(_, _, extra) => Left(extra.traced.trace)
  }
}
