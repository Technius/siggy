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

  it should "find methods in nested objects" in {
    val Right(sigs) = Siggy.analyze("""object Foo {
      object Bar {
        def baz(a: Int): String = a.toString
      }
    }
    """)
    val Right(query) = Query.parse("Int => String")
    Siggy.querySignatures(sigs, query) should not be empty
  }

  it should "find generic methods" in {
    val Right(sigs) = Siggy.analyze("""object Foo {
      def optionMap[A, B](oa: Option[A])(f: A => B): Option[B] = os.map(f)
    }
    """)
    val Right(query) = Query.parse("[A,B] Option[A] => Option[B]")
    Siggy.querySignatures(sigs, query) should not be empty
  }
}
