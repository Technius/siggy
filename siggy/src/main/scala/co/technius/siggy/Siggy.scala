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
        val pkgs = tree collect { case q"package $name { ..$stats }" => (name, stats) }
        val defs = pkgs flatMap { case (name, stats) => analyzePkg(name.syntax, stats) }
        Right(defs)
      case Parsed.Error(_, _, details) =>
        Left(details.getMessage)
    }
  }

  /**
    * Searches a package for method definitions.
    */
  def analyzePkg(pkg: String, stats: List[Stat]): List[Signature] = {
    val containers = stats collect {
      case obj: Defn.Object => obj
      case cls: Defn.Class => cls
      case trt: Defn.Trait => trt
    }
    containers.flatMap(findSignatures(pkg))
  }

  /**
    * Searches the syntax tree for method definitions, returning a method
    * signature.
    * @param enclosing The enclosing class or method.
    * @param sep The separator between the class/method and the method definition.
    * @param tree The syntax tree
    */
  def findSignatures(pkg: String)(tree: Tree): List[Signature] = {
    val prefix = pkg + (tree match {
      case obj: Defn.Object => "." + obj.name + "."
      case cls: Defn.Class => "." + cls.name + "#"
      case trt: Defn.Trait => "." + trt.name + "#"
      case _ => ""
    })
    tree collect {
      case q"..$mods def $name[..$tparams](..$params): ${Some(tpe)} = $expr" =>
        val sigParams = params.map(p => (p.name.toString, p.decltpe.get.syntax))
        Signature(prefix, name.syntax, tparams.map(_.syntax), sigParams, tpe.syntax)
    }
  }

  def querySignatures(sigs: List[Signature], query: Query): List[Signature] =
    sigs filter { s =>
      val tparamsMatch = s.tparams == query.tparams
      val paramsMatch = s.params.map(_._2) == query.params.init
      val tpeMatch = s.tpe == query.params.last
      tpeMatch && tparamsMatch && paramsMatch
    }
}

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
  params: List[(String, String)],
  tpe: String) {

  override def toString: String = {
    val tparamStr = if (tparams.isEmpty) "" else tparams.mkString("[", ",", "]")
    val paramStr = params.map(p => p._1 + ": " + p._2).mkString("(", ", ", ")")
    s"$prefix$name$tparamStr$paramStr: $tpe"
  }
}
