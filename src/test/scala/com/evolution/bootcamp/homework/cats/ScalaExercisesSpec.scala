package com.evolution.bootcamp.homework.cats

import cats._
import cats.data.{OptionT, Validated, ValidatedNel}
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ScalaExercisesSpec extends AnyFunSuite{

  test("Semigroup") {
    Semigroup[Int].combine(1, 2) should be(3)
    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
    Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(Some(3))
    Semigroup[Option[Int]].combine(Option(1), None) should be(Some(1))

    Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) should be(67)

    val aMap = Map("foo" -> Map("bar" -> 5))
    val anotherMap = Map("foo" -> Map("bar" -> 6))
    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

    combinedMap.get("foo") should be(Some(Map("bar" -> 11)))
  }
  test("Monoid") {
    Monoid[String].empty should be("")
    Monoid[String].combineAll(List("a", "b", "c")) should be("abc")
    Monoid[String].combineAll(List()) should be("")

    Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) should be(Map("a" -> 4, "b" -> 2))
    Monoid[Map[String, Int]].combineAll(List()) should be(Map())

    val l = List(1, 2, 3, 4, 5)
    l.foldMap(identity) should be(15)
    l.foldMap(i => i.toString) should be("12345")

    val l1 = List(1, 2, 3, 4, 5)
    l1.foldMap(i => (i, i.toString)) should be((15, "12345"))
  }
  test("Functor") {
    Functor[Option].map(Option("Hello"))(_.length) should be(Option(5))
    Functor[Option].map(None: Option[String])(_.length) should be(None)

    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("Hello")) should be(Option(5))

    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    product.get("Cats").getOrElse(0) should be(4)
    product.get("is").getOrElse(0) should be(2)
    product.get("awesome").getOrElse(0) should be(7)

    val listOpt = Functor[List] compose Functor[Option]
    listOpt.map(List(Some(1), None, Some(3)))(_ + 1) should be(List(Some(2), None, Some(4)))

  }
  test("Apply") {
    val intToString: Int => String = _.toString
    val double: Int => Int = _ * 2
    val addTwo: Int => Int = _ + 2

    Apply[Option].map(Some(1))(intToString) should be(Some("1"))
    Apply[Option].map(Some(1))(double) should be(Some(2))
    Apply[Option].map(None)(addTwo) should be(None)

    val listOpt = Apply[List] compose Apply[Option]
    val plusOne = (x: Int) => x + 1
    listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) should be(List(Some(2), None, Some(4)))

    Apply[Option].ap(Some(intToString))(Some(1)) should be(Some("1"))
    Apply[Option].ap(Some(double))(Some(1)) should be(Some(2))
    Apply[Option].ap(Some(double))(None) should be(None)
    Apply[Option].ap(None)(Some(1)) should be(None)
    Apply[Option].ap(None)(None) should be(None)

    val addArity2 = (a: Int, b: Int) => a + b
    Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) should be(Some(3))
    Apply[Option].ap2(Some(addArity2))(Some(1), None) should be(None)

    val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
    Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) should be(Some(6))

    Apply[Option].map2(Some(1), Some(2))(addArity2) should be(Some(3))

    Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3) should be(Some(6))

    Apply[Option].tuple2(Some(1), Some(2)) should be(Some((1, 2)))
    Apply[Option].tuple3(Some(1), Some(2), Some(3)) should be(Some((1, 2, 3)))

    val option2 = (Option(1), Option(2))
    val option3 = (option2._1, option2._2, Option.empty[Int])

    option2 mapN addArity2 should be(Option(3))
    option3 mapN addArity3 should be(None)

    option2 apWith Some(addArity2) should be(Option(3))
    option3 apWith Some(addArity3) should be(None)

    option2.tupled should be(Option((1, 2)))
    option3.tupled should be(None)
  }
  test("Applicative") {
    Applicative[Option].pure(1) should be(Some(1))
    Applicative[List].pure(1) should be(List(1))

    (Applicative[List] compose Applicative[Option]).pure(1) should be(List(Some(1)))

    Monad[Option].pure(1) should be(Some(1))
    Applicative[Option].pure(1) should be(Some(1))
  }
  test("Monad") {
    Option(Option(1)).flatten should be(Option(1))
    Option(None).flatten should be(None)
    List(List(1), List(2, 3)).flatten should be(List(1, 2, 3))

    Monad[Option].pure(42) should be(Some(42))

    Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) should be(List(1, 1, 2, 2 ,3 ,3))

    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be(Option("truthy"))
    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4, 1, 2))

//  optionTMonad[List].pure(42) should be(OptionT(List(Some(42))))
  }
  test("Foldable") {
    Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) should be(6)
    Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) should be("abc")

    val lazyResult = Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) => Later(x + rest.value))
    lazyResult.value should be(6)

    Foldable[List].fold(List("a", "b", "c")) should be("abc")
    Foldable[List].fold(List(1, 2, 3)) should be(6)

    Foldable[List].foldMap(List("a", "b", "c"))(_.length) should be(3)
    Foldable[List].foldMap(List(1, 2, 3))(_.toString) should be("123")

    Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) should be(List(1, 2, 3, 4, 5))
    Foldable[List].foldK(List(None, Option("two"), Option("three"))) should be(Option("two"))

    Foldable[List].find(List(1, 2, 3))(_ > 2) should be(Some(3))
    Foldable[List].find(List(1, 2, 3))(_ > 5) should be(None)

    Foldable[List].exists(List(1, 2, 3))(_ > 2) should be(true)
    Foldable[List].exists(List(1, 2, 3))(_ > 5) should be(false)

    Foldable[List].forall(List(1, 2, 3))(_ <= 3) should be(true)
    Foldable[List].forall(List(1, 2, 3))(_ < 3) should be(false)

    Foldable[List].toList(List(1, 2, 3)) should be(List(1, 2, 3))
    Foldable[Option].toList(Option(42)) should be(List(42))
    Foldable[Option].toList(None) should be(Nil)

    Foldable[List].filter_(List(1, 2, 3))(_ < 3) should be(List(1, 2))
    Foldable[Option].filter_(Option(42))(_ != 42) should be(Nil)


    def parseInt(s: String): Option[Int] =
      Either.catchOnly[NumberFormatException](s.toInt).toOption

    Foldable[List].traverse_(List("1", "2", "3"))(parseInt) should be(Some())
    Foldable[List].traverse_(List("a", "b", "c"))(parseInt) should be(None)

    val FoldableListOption = Foldable[List].compose[Option]
    FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) should be(10)
    FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) should be("123")

    Foldable[List].isEmpty(List(1, 2, 3)) should be(false)
    Foldable[List].dropWhile_(List(1, 2, 3))(_ < 2) should be(List(2, 3))
    Foldable[List].takeWhile_(List(1, 2, 3))(_ < 2) should be(List(1))
  }
  test("Traverse") {

    def parseIntEither(s: String): Either[NumberFormatException, Int] =
      Either.catchOnly[NumberFormatException](s.toInt)

    def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
      Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

    List("1", "2", "3").traverse(parseIntEither) should be(Right(List(1, 2, 3)))
    List("1", "abc", "3").traverse(parseIntEither).isLeft should be(true)

    List("1", "2", "3").traverse(parseIntValidated).isValid should be(true)

    List(Option(1), Option(2), Option(3)).traverse(identity) should be(Option(List(1, 2, 3)))
    List(Option(1), None, Option(3)).traverse(identity) should be(None)

    List(Option(1), Option(2), Option(3)).sequence_ should be(Option(()))
    List(Option(1), None, Option(3)).sequence_ should be(None)
  }
  test("Identity") {
    type Id[A] = A

    val anId: Id[Int] = 42
    anId should be(42)

    Applicative[Id].pure(42) should be(42)

    val fortytwo: Int = 42
    Comonad[Id].coflatMap(fortytwo)(_ + 1) should be(43)
  }
}
