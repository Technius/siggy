import org.scalatest._

import co.technius.siggy._

class QueryTest extends FlatSpec with Matchers {
  "Queries" should "support normal functions" in {
    val result = Query.parse("Int => String")
    result shouldBe a [Right[_, _]]
    val Right(query) = result
    query.tparams.length should === (0)
    query.params.length should === (2)
  }

  it should "support type parameters" in {
    val result = Query.parse("[A,B] Option[A] => Option[B]")
    result shouldBe a [Right[_, _]]
    val Right(query) = result
    query.tparams.length should === (2)
    query.params.length should === (2)
  }

  it should "support higher-kinded types" in {
    val result = Query.parse("[F[_], A] Monoid[F[_]] => A => F[A]")
    result shouldBe a [Right[_, _]]
    val Right(query) = result
    query.tparams.length should === (2)
    query.params.length should === (3)
    // F[_] should be a type constructor
    query.tparams.head.tparams.length should === (1)
    // Monoid[F[_]] should have F[_]
    query.params.head.tparams.length should === (1)
  }

  it should "support function types" in {
    val Right(query1) = Query.parse("[A, B, C] (A => B) => (B => C) => (A => C)")
    query1.tparams.length should === (3)
    query1.params.length should === (3)
    val Right(query2) = Query.parse("[A, B] Option[A] => Option[A => B] => Option[B]")
    query2.tparams.length should === (2)
    query2.params.length should === (3)
    val Right(query3) = Query.parse("[A, B] Option[A] => (A => B) => Option[B]")
    query3.tparams.length should === (2)
    query3.params.length should === (3)
  }
}
