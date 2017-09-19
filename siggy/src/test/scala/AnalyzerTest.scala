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

  it should "find defs in nested objects and classes" in {
    val result = Siggy.analyze("""class Foo {
      class Bar {
        def baz(a: Int): Int = a
      }
    }
    object Lorem {
      class Ipsum {
        def dolor(a: String): String = a
      }
      object Sit {
        def amet(a: Boolean): Boolean = a
      }
    }
    """)
    result shouldBe a [Right[_, _]]
    val Right(sigs) = result
    sigs.length should === (3)
    val List(bz, dlr, amet) = sigs
    bz.prefix should === ("Foo.Bar#")
    dlr.prefix should === ("Lorem.Ipsum#")
    amet.prefix should === ("Lorem.Sit.")
  }

  it should "find defs in package objects" in {
    val result = Siggy.analyze("""package object foo {
      def bar(a: Int): Int = a
      def baz(a: String): String = a
    }
    """)
    result shouldBe a [Right[_, _]]
    val Right(sigs) = result
    sigs.length should === (2)
  }
}
