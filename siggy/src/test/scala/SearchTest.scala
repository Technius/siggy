import org.scalatest._

import co.technius.siggy._

class SearchTest extends FlatSpec with Matchers {
  "Siggy search" should "find class members" in {
    val Right(sigs) = Siggy.findDefs("""class Foo {
      def bar: Int = 1
    }
    """)
    val Right(query) = Query.parse("Foo => Int")
    Siggy.querySignatures(sigs, query) should not be empty
  }

  it should "find methods in nested objects" in {
    val Right(sigs) = Siggy.findDefs("""object Foo {
      object Bar {
        def baz(a: Int): String = a.toString
      }
    }
    """)
    val Right(query) = Query.parse("Int => String")
    Siggy.querySignatures(sigs, query) should not be empty
  }

  it should "find generic methods" in {
    val Right(sigs) = Siggy.findDefs("""object Foo {
      def optionMap[A, B](oa: Option[A])(f: A => B): Option[B] = os.map(f)
    }
    """)
    val Right(query) = Query.parse("[A,B] Option[A] => (A => B) => Option[B]")
    Siggy.querySignatures(sigs, query) should not be empty
  }

  it should "find function types" in {
    val Right(sigs) = Siggy.findDefs("""object Foo {
      def compose[A, B, C](g: B => C)(f: A => B): A => C = ???
    }
    """)
    val Right(query) = Query.parse("[A, B, C] (B => C) => (A => B) => (A => C)")
    Siggy.querySignatures(sigs, query) should not be empty
  }

  it should "find higher-kinded types" in {
    val Right(sigs) = Siggy.findDefs("""object Foo {
      def flatten[F[_], A](ffa: F[F[A]]): F[A] = ???
    }
    """)
    val Right(query) = Query.parse("[F[_], A] F[F[A]] => F[A]")
    Siggy.querySignatures(sigs, query) should not be empty
  }

  it should "find tuple types" in {
    val Right(sigs) = Siggy.findDefs("""object Foo {
      def fst[A,B](t: (A, B)): A = ???
    }
    """)
    val Right(query) = Query.parse("[A, B] (A, B) => A")
    Siggy.querySignatures(sigs, query) should not be empty
  }

  it should "flatten function and tuple types" in {
    val Right(sigs) = Siggy.findDefs("""object Foo {
      def foldLeft[A, B](f: (B, A) => B)(z: B)(xs: List[A]): B = ???
    }
    """)
    val queries = Seq(
      "[A, B] B => A => B => B => List[A] => B",
      "[A, B] (B => A => B) => B => List[A] => B",
      "[A, B] ((B, A) => B) => B => List[A] => B"
    )
    queries foreach { q =>
      val Right(query) = Query.parse(q)
      Siggy.querySignatures(sigs, query) should not be empty
    }
  }
}
