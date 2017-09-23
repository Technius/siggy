package co.technius.siggy

/**
  * Represents a method signature.
  * @param prefix The enclosing class, trait, or object, followed by a "." or a "#".
  * @param tparams Type parameters
  * @param params A list of parameter names and types
  * @param tpe  The return type
  */
case class Signature(
  prefix: String,
  name: String,
  tparams: List[String],
  paramLists: List[List[(String, String)]],
  tpe: String,
  property: Boolean = false) {

  def enclosingName: Option[String] = {
    val i = prefix.lastIndexOf(".")
    // last character is '.' or '#', so we need to remove it
    if (i == -1 && prefix.length > i + 1) {
      if (prefix.isEmpty) None else Some(prefix.init)
    } else {
      Some(prefix.slice(i + 1, prefix.length - 1))
    }
  }

  override def toString: String = {
    val tparamStr = if (tparams.isEmpty) "" else tparams.mkString("[", ",", "]")
    val paramStr =
      if (paramLists.isEmpty && property) ""
      else paramLists.map(pl =>
        pl.map(p => p._1 + ": " + p._2).mkString("(", ", ", ")")).mkString
    s"$prefix$name$tparamStr$paramStr: $tpe"
  }
}
