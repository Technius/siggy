import org.scalatest._

import co.technius.siggy._

class SearchTest extends FlatSpec with Matchers {
  "Siggy search" should "find class members" in {
    val Right(sigs) = Siggy.analyze("""class Foo {
      def bar: Int = 1
    }
    """)
    val Right(query) = Query.parse("Foo => Int")
    Siggy.querySignatures(sigs, query) should not be empty
  }
}
