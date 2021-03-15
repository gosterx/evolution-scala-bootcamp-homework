package com.evolution.bootcamp.homework.typeclass

object QAndASession extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A: Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]
  }

  object syntaxSemigroup {
    implicit class SemigroupOps[A: Semigroup](x: A) {
      def combine(y: A): A = {
        Semigroup[A].combine(x, y)
      }
    }
  }

  import syntaxSemigroup._

  // 1.2. Implement Semigroup for Long, String

  implicit val semigroupLong: Semigroup[Long] = new Semigroup[Long] {
    override def combine(x: Long, y: Long): Long = x + y
  }

  implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit val semigroupString: Semigroup[String] = new Semigroup[String] {
    override def combine(x: String, y: String): String = x + y
  }

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists

  def combineAll[A: Semigroup](list: List[A]): A = list.reduce(_ combine _)

  // combineAll(List(1L, 2L, 3L)) == 6

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists

  def combineAll[A: Semigroup](list: List[A], startingElement: A): A = {
    list.foldLeft(startingElement)(_ combine _)
  }

  // println(combineAll(List(1, 2, 3), 0) == 6)
  // println(combineAll(List(), 1) == 1)

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }

  // 2.2. Implement Monoid for Long, String

  implicit val monoidLong: Monoid[Long] = new Monoid[Long] {
    override val empty: Long = 0
    override def combine(x: Long, y: Long): Long = x + y
  }

  implicit val monoidString: Monoid[String] = new Monoid[String] {
    override val empty: String = ""
    override def combine(x: String, y: String): String = x + y
  }

  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
    override val empty: Int = 0
    override def combine(x: Int, y: Int): Int = x + y
  }


  // 2.3. Implement combineAll(list: List[A]) for all lists

  def combineAllMonoid[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)((x, y) => Monoid[A].combine(x, y))

  //  println(combineAllMonoid(List(1, 2, 3)) == 6)

  // 2.4. Implement Monoid for Option[A]

  implicit def monoidOption[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(xVal), Some(yVal)) => Some(Semigroup[A].combine(xVal, yVal))
      case (x, y) => x.orElse(y)
    }
  }

  // combineAll(List(Some(1), None, Some(3))) == Some(4)
  // combineAll(List(None, None)) == None
  // combineAll(List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)

  implicit def monoidFunction1[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override def empty: A => B = _ => Monoid[B].empty

    override def combine(x: A => B, y: A => B): A => B = p => Semigroup[B].combine(x(p), y(p))
  }

  combineAll(List((a: String) => a.length, (a: String) => a.toInt))               //=== (a: String) => (a.length + a.toInt)
  combineAll(List((a: String) => a.length, (a: String) => a.toInt)).apply("123")  //=== 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)

  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly[Semigroupal[F]]
  }

  implicit class SemigroupalOps[F[_]: Semigroupal, A](fa: F[A]) {
    def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
  }

  // 4.2. Implement Semigroupal for Option

  implicit val semigroupalOption: Semigroupal[Option] = new Semigroupal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(faVal), Some(fbVal)) => Some((faVal, fbVal))
      case _                          => None
    }
    //      for {
    //        a <- fa
    //        b <- fb
    //      } yield (a, b)
  }

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]

  implicit class Tuple2Ops[F[_]: Functor : Semigroupal, A, B](t: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = {
      t match {
        case (x, y) => x.product(y).fmap(f.tupled) // (f.tupled) === { case (a, b) => f(a, b) }
      }
    }
  }

  // (Option(1), Option(2)).mapN(_ + _) == Some(3)
  // (Option(1), None).mapN(_ + _)      == None


  // 4.4. Implement Semigroupal for Map

  implicit def semigroupalMap[T]: Semigroupal[Map[T, *]] = new Semigroupal[Map[T, *]] {
    override def product[A, B](fa: Map[T, A], fb: Map[T, B]): Map[T, (A, B)] =
      for {
        (faK, faV) <- fa
        fbV <- fb.get(faK)
      } yield (faK, (faV, fbV))
  }

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  object Applicative {
    def apply[F: Applicative]: Applicative[F] = implicitly[Applicative[F]]
  }

  // 5.1. Implement Applicative for Option, Either

  new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(x) => Some(f(x))
      case None => None
    }

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(faVal), Some(fbVal)) => Some((faVal, fbVal))
      case _                          => None
    }
  }

  new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)
    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = (fa, fb) match {
      case (Right(x), Right(y)) => Right(x, y)
      case (Left(x), _) => Left(x)
      case (_, Left(y)) => Left(y)
    }
    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = fa match {
      case Right(x) => Right(f(x))
      case Left(x)  => Left(x)
    }
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???

  def traverseApplicative[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldLeft(Applicative[F].pure(List.empty[B]))((acc, a) => (acc, f(a)).mapN((x, y) => x.::(y)))
  }

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`

  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A], z: B)(op: (B, A) => B): B

    //just for task 6.3
    def empty[A]: F[A]
    def concat[A](fa: F[A], x: A): F[A] // fa :: x
  }
  object Foldable {
    def apply[F: Foldable]: Foldable[F] = implicitly[Foldable[F]]
  }
  implicit class foldableOps[F[_]: Foldable, A](x: F[A]) {
    def foldLeft[B](z: B)(op: (B, A) => B): B = Foldable[F].foldLeft(x, z)(op)
  }
  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library

  implicit val foldableList: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], z: B)(op: (B, A) => B): B = fa.foldLeft(z)(op)


    override def empty[A]: List[A] = ???
    override def concat[A](fa: List[A], x: A): List[A] = ???
  }

  // 6.3. Implement `traverse` for all Foldables instead of List

  def traverseFoldable[T[_]: Foldable, F[_]: Applicative, A, B](as: T[A])(f: A => F[B]): F[T[B]] = {
    as.foldLeft(Applicative[F].pure(Foldable[T].empty))((acc, a) => (acc, f(a)).mapN((x, y) => Foldable[T].concat(x, y)))
  }

}
