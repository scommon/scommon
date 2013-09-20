package org.scommon.reactive

import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers

import scala.collection._

@RunWith(classOf[JUnitRunner])
class GeneratorTest extends FunSuite
                     with ShouldMatchers
                     with SeveredStackTraces {

  /* For coolness' sake.

    //Should reject invariant things, and still be covariant, although you will now have the least upper bound on your type so if you have List[A] it should always be inferred as A

    import shapeless._
    import scala.language.higherKinds
    import scala.language.implicitConversions
    trait Foo[+TFoo]
    class Bar[TFooNext, TFooHead[+_] <: Foo[_], TFooTail <: HList](val building: TFooHead[TFooNext] :: TFooTail) {
      def next[TNext, TFoo[+_] <: Foo[_]](n: TFoo[TNext]) = new Bar[TNext, TFoo, TFooHead[TFooNext] :: TFooTail](n :: building)
    }
    implicit def foo2Bar[TNext, TFoo[+_] <: Foo[_]](f: TFoo[TNext]) = new Bar[TNext, TFoo, HNil](f :: HNil)
    val ff = new Foo[Int] {}
    val e = ff next new Foo[String] {}
    val h = e.building.head
  */

  /* Slightly more complicated craziness...

    import shapeless._
    import scala.language.higherKinds
    import scala.language.implicitConversions

    trait Foo[+TFoo]
    type Baz[THList <: HList] = THList => Unit

    class Bar[TFooNext, TFooHead[+_] <: Foo[_], TFooTail <: HList, TBazHead[+_] <: Option[_], TBazTail <: HList](val building: TFooHead[TFooNext] :: TFooTail, val combines: TBazHead[TFooNext] :: TBazTail) {
      type TBuilding = TFooHead[TFooNext] :: TFooTail
      type TCombine = TBazHead[TFooNext] :: TBazTail

      def next[TNext, TFoo[+_] <: Foo[_], TBaz[+_] <: Option[_]](n: TFoo[TNext], default: TBaz[TNext] = None) =
        new Bar[TNext, TFoo, TBuilding, TBaz, TCombine](n :: building, default :: combines)
      def combine(fn: Baz[TBuilding]): Unit = ???
    }
    implicit def foo2Bar[TNext, TFoo[+_] <: Foo[_], TBaz[+_] <: Option[_]](f: TFoo[TNext], default: TBaz[TNext] = None) = new Bar[TNext, TFoo, HNil, TBaz, HNil](f :: HNil, default :: HNil)
      //^^^ works!

    import Zipper._
    def test[W, X <: HList, Y <: HList, Z](a: Zipper[W, X, Y, Z]): Unit = {
      val left = a.left
      println(left)
      test(a.right.toZipper)
    }

    val ff = new Foo[Int] {}
    val e = foo2Bar(ff) next new Foo[String] {}
    val h = e.building.head

    val r = e.building.toZipper
    test(e.building.toZipper)
  */
  test("Generator API works correctly") {
    import Generator._
    var received = false

    val right_associativity: Junction[Nothing] = {
      (x: Any) =>
        if (received)
          fail("Should only be called once when no extra data has been pushed.")
        received = true
        x should be(Seq(Some(Seq("x", "y", "z")), Some(Seq(1, 2, 3)))) //Due to right associativity
      } <<: waitForAll <<|: (Seq(1, 2, 3) ~: Seq("x", "y", "z"))
    right_associativity.begin
    received should be(true)

    received = false
    val left_associativity: Junction[Nothing] =
      (Seq(1, 2, 3) ~ Seq("x", "y", "z") |>> waitForAll) >> { x: Any =>
        if (received)
          fail("Should only be called once when no extra data has been pushed.")
        received = true
        x should be(Seq(Some(Seq(1, 2, 3)), Some(Seq("x", "y", "z")))) //Due to left associativity
      }
    left_associativity.begin
    received should be(true)

    intercept[IllegalArgumentException] {
      //Cannot call begin a second time on a generator.
      (left_associativity - waitForAll).begin
    }

    val g1: Generator[Int, Nothing] = Seq(1, 2, 3)
    val g2: Generator[String, Nothing] = Seq("x", "y", "z")

    val embrace1 = (g1 ~ g2 |>> waitForAll) >> { x: Any => ??? }

    val embrace1_missing = embrace1 - waitForAll - g1 - g2
    embrace1_missing.associated_barriers should be(Seq())
    embrace1_missing.associated_embrace.generators should be(Seq())


    case class Pass(expected: Any)

    val pass_filter_1 = mutable.Queue(
        Pass(Some(Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(None,Seq())))))
      , Pass(Some(Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(Some(Seq("x", "y", "z")),Seq())))))
    )

    val pass_filter_2 = mutable.Queue(
        Pass(Some(Seq(Some(Received(Some(Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(None,Seq())))),Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(None,Seq()))))), Some(Received(None,Seq())), Some(Received(None,Seq())))))
      , Pass(Some(Seq(Some(Received(Some(Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(Some(Seq("x", "y", "z")),Seq())))),Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(Some(Seq("x", "y", "z")),Seq()))))), Some(Received(None,Seq())), Some(Received(None,Seq())))))
      , Pass(Some(Seq(Some(Received(Some(Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(Some(Seq("x", "y", "z")),Seq())))),Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(Some(Seq("x", "y", "z")),Seq()))))), Some(Received(Some(Seq("a")),Seq())), Some(Received(None,Seq())))))
      , Pass(Some(Seq(Some(Received(Some(Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(Some(Seq("x", "y", "z")),Seq())))),Seq(Some(Received(Some(Seq(1, 2, 3)),Seq())), Some(Received(Some(Seq("x", "y", "z")),Seq()))))), Some(Received(Some(Seq("a")),Seq())), Some(Received(Some(Seq("b")),Seq())))))
    )

    var junction_passes_1, junction_passes_2 = 4

    val c1 = Seq(1, 2, 3) ~ Seq("x", "y", "z")
    val c2 = c1 |>> ((_, x: Received) => {
      x.data should be(pass_filter_1.dequeue.expected)
      //println(s"filter ${x.data} \n");
      true
    })
    val c3 = c2 ~ Seq("a") ~ Seq("b")
    val c4 = c3 |>> ((_, x: Received) => {
      x.data should be(pass_filter_2.dequeue.expected)
      //println(s"second filter ${x.data} \n")
      true
    })
    val c5 = c4 >> ((_, x) => {
      junction_passes_1 -= 1
      //println(s"junction: $x \n")
    })
    val c6 = c5 >> ((_, x:Any) => {
      junction_passes_2 -= 1
      //println(s"x: $x")
    })

    c6.begin

    junction_passes_1 should be(0)
    junction_passes_2 should be(0)
  }

}
