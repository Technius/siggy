package co.technius.siggy

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.meta._
import scala.util.{Failure, Success, Try}

object Siggy {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case path :: Nil =>
        scanPath(path) foreach { path =>
          withSource(path) { s =>
            analyze(s.mkString) match {
              case Left(err) => println("Error: " + err)
              case Right(sigs) =>
                if (!sigs.isEmpty) {
                  println(path.toString + ":")
                  sigs.foreach(println _)
                }
            }
          }
        }
      case path :: query :: Nil =>
        Query.parse(query) match {
          case Left(err) => println("Invalid query: " + err)
          case Right(query) =>
            scanPath(path) foreach { path =>
              withSource(path) { s =>
                analyze(s.mkString) match {
                  case Left(err) => println("Error: " + err)
                  case Right(sigs) =>
                    val matches = querySignatures(sigs, query)
                    if (!matches.isEmpty) {
                      println(path.toString + ":")
                      matches.foreach(println _)
                    }
                }
              }
            }
        }
      case _ => println("Usage: siggy <file> [query]")
    }
  }

  /**
    * Searches a path for all *.scala files
    */
  def scanPath(path: String): Stream[Path] = {
    val start = Paths.get(path)
    val jstream = Files.walk(start)
    jstream.iterator().asScala.toStream.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".scala"))
  }

  def withSource(path: Path)(f: String => Unit): Unit = {
    val src = Try(scala.io.Source.fromInputStream(Files.newInputStream(path)))
    src match {
      case Success(s) => f(s.mkString)
      case Failure(e) => println("Error: " + e.getMessage)
    }
  }

  /**
    * Attempts to find the method definitions in the given source code.
    */
  def analyze(src: String): Either[String, List[Signature]] = {
    src.parse[Source] match {
      case Parsed.Success(tree) =>
        val defs = analyzeStats("", tree.stats)
        Right(defs)
      case Parsed.Error(_, _, details) =>
        Left(details.getMessage)
    }
  }

  /**
    * Recursively searches a list of statements for method definitions.
    * @param pkgName The name of the enclosing package, or empty string if there is none.
    * @param stats The list of statements to search.
    */
  def analyzeStats(pkgName: String, stats: List[Stat]): List[Signature] = {
    val prefix = if (pkgName.isEmpty) "" else pkgName + "."
    def recurContainer(name: String, sep: String, st: List[Stat]): List[Signature] =
      findSignatures(prefix + name + sep)(st) ++ analyzeStats(prefix + name, st)
    stats flatMap {
      case pkg: Pkg =>
        analyzeStats(prefix + pkg.name, pkg.stats)
      case po: Pkg.Object => recurContainer(po.name.toString, ".", po.templ.stats)
      case obj: Defn.Object => recurContainer(obj.name.toString, ".", obj.templ.stats)
      case cls: Defn.Class => recurContainer(cls.name.toString, "#", cls.templ.stats)
      case trt: Defn.Trait => recurContainer(trt.name.toString, "#", trt.templ.stats)
      case _ => List.empty
    }
  }

  /**
    * Searches a list of statements for method definitions, returning a method
    * signature.
    * @param enclosing The enclosing class or method.
    * @param sep The separator between the class/method and the method definition.
    * @param tree The syntax tree
    */
  def findSignatures(prefix: String)(stats: List[Stat]): List[Signature] = {
    stats collect {
      case Defn.Def(mods, name, tparams, paramLists, Some(tpe), _) =>
        val pls = paramLists.map(pl => pl.map(p => (p.name.toString, typeToInfo(p.decltpe.get))))
        def tparamToInfo(p: Type.Param): TypeInfo =
          TypeInfo(p.name.toString, p.tparams.map(tparamToInfo(_)))
        Signature(prefix, name.toString, tparams.map(tparamToInfo(_)), pls, typeToInfo(tpe))
    }
  }

  def querySignatures(sigs: List[Signature], query: Query): List[Signature] =
    sigs filter { s =>
      val defParams: Seq[TypeInfo] = s.paramLists.flatten.map(_._2) :+ s.tpe
      val queryParams = query.params

      // match type paramemter length
      // this is because e.g. `[A,B] A => B` and `[C,D] C => D` are equivalent
      val tparamsLenMatch = s.tparams.length == query.tparams.length
      // match type signature
      val sigMatch = defParams == queryParams
      // match type signature with enclosing class/object/trait prepended
      // e.g. Foo => Int matches Foo.foo: Int
      lazy val sigWithEncMatch =
        s.enclosingName.map(TypeInfo(_, Seq.empty)).toList ++ defParams == queryParams
      tparamsLenMatch && (sigMatch || sigWithEncMatch)
    }

  def typeToInfo(tpe: Type): TypeInfo = tpe match {
    case t: Type.Name => TypeInfo(t.toString, Seq.empty)
    case t: Type.Apply => TypeInfo(t.tpe.toString, t.args.map(typeToInfo(_)))
    case t: Type.Function => TypeInfo("=>", t.params.map(typeToInfo(_)) :+ typeToInfo(t.res))
    case _ => sys.error("typeToInfo not implemented for " + tpe.getClass.getName)
  }
}
