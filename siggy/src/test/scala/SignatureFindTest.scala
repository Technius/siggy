import org.scalatest._

import co.technius.siggy._

class SignatureFindTest extends FlatSpec with Matchers {
  "Siggy" should "find defs in top-level objects" in {
    val result = Siggy.findDefs("""object Foo {
      def foo(a: Int): String = a.toString
      def bar: Int = 1
    }
    """)
    val Right(sigs) = result
    sigs.length should === (2)
  }

  it should "find property defs" in {
    val result = Siggy.findDefs("""object Foo1 {
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
    val result = Siggy.findDefs("""class Foo {
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
    val Right(sigs) = result
    sigs.length should === (3)
    val List(bz, dlr, amet) = sigs
    bz.prefix should === ("Foo.Bar#")
    dlr.prefix should === ("Lorem.Ipsum#")
    amet.prefix should === ("Lorem.Sit.")
  }

  it should "find defs in package objects" in {
    val result = Siggy.findDefs("""package object foo {
      def bar(a: Int): Int = a
      def baz(a: String): String = a
    }
    """)
    val Right(sigs) = result
    sigs.length should === (2)
  }

  it should "find generic methods" in {
    val result = Siggy.findDefs("""object Foo {
      def id[A](a: A): A = a
      def const[A, B](a: A)(ignored: B): A = a
      def bar[F[_], A, B](a: F[A]): F[A] = a
    }
    """)
    val Right(List(idSig, constSig, foo)) = result
    idSig.tparams.length should === (1)
    idSig.tparams(0) should === (TypeInfo("A", Seq.empty))
    constSig.tparams.length should === (2)
    constSig.tparams(0) should === (TypeInfo("A", Seq.empty))
    constSig.tparams(1) should === (TypeInfo("B", Seq.empty))
  }

  it should "find methods involving tuples" in {
    val result = Siggy.findDefs("""object Foo {
      def curry[A,B,C](f: (A, B) => C): A => B => C = ???
      def uncurry[A,B,C](f: A => B => C): (A, B) => C = ???
    }
    """)
    val Right(List(currySig, uncurrySig)) = result
    currySig.tparams.length should === (3)
    currySig.paramLists(0).length should === (1)
    uncurrySig.tparams.length should === (3)
    uncurrySig.paramLists(0).length should === (1)
  }
}
