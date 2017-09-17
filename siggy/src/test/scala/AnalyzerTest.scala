import org.scalatest._

import co.technius.siggy._

class AnalyzerTest extends FlatSpec with Matchers {
  "Siggy analyzer" should "find defs in top-level objects" in {
    val result = Siggy.analyze("""object Foo {
      def foo(a: Int): String = a.toString
      def bar: Int = 1
    }
    """)
    result shouldBe a [Right[_, _]]
    val Right(sigs) = result
    sigs.length should === (2)
  }

  it should "find property defs" in {
    val result = Siggy.analyze("""object Foo1 {
      def bar: Int = 1
    }
    trait Foo2 {
      def bar: Int = 2
    }
    class Foo3 {
      def bar: Int = 3
    }
    """)
    result shouldBe a [Right[_, _]]
    val Right(sigs) = result
    sigs.length should === (3)
  }
}
