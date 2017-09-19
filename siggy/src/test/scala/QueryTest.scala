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
}
