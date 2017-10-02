package co.technius.siggy

/**
  * Represents a method signature.
  * @param prefix The enclosing class, trait, or object, followed by a "." or a "#".
  * @param tparams Type parameters
  * @param params A list of parameter names and types
  * @param tpe  The return type
  * @param lineNum The line number at which the signature begins.
  */
case class Signature(
  prefix: String,
  name: String,
  tparams: List[TypeInfo],
  paramLists: List[List[(String, TypeInfo)]],
  tpe: TypeInfo,
  lineNum: Int) {

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
      if (paramLists.isEmpty) ""
      else paramLists.map(pl =>
        pl.map(p => p._1 + ": " + p._2).mkString("(", ", ", ")")).mkString
    s"line $lineNum: $prefix$name$tparamStr$paramStr: $tpe"
  }
}
