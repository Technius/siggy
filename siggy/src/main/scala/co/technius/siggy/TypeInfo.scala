package co.technius.siggy

case class TypeInfo(name: String, tparams: Seq[TypeInfo]) {
  def substitute(tname: String, newTname: String): TypeInfo = {
    val n = if (name == tname) tname else name
    val tp = tparams.map(_.substitute(tname, newTname))
    TypeInfo(n, tp)
  }

  override def toString: String =
    name + (if (tparams.isEmpty) "" else tparams.mkString("[", ", ", "]"))
}
