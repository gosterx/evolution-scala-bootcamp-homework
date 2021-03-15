package com.evolution.bootcamp.homework.typeclass

object Implicits extends App {

  // having two implementations for the same type (like different ways to make json out of User) is possible
  // but considered to be bad

  object TypeclassTask {

    // Why am I not a Typeclass?
    // TODO: Rework me so I am a typeclass
    // TODO: make an instance for String
    // TODO: write "abc".hash to check everything
    trait HashCode[T] {
      def hash(entity: T): Int
    }

    object HashCode {
      def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
    }

    implicit class HashCodeSyntax[A](x: A) {
      def hash(implicit h: HashCode[A]): Int = {
        h.hash(x)
      }
    }

    implicit val hashCodeInstance: HashCode[String] = str => str.hashCode

  }

  object Task1 {

    // TODO: create Ordering instance for Money
    final case class Money(amount: BigDecimal)

    implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount.toInt - y.amount.toInt
  }

  object Task2 {

    // TODO: create Show instance for User
    // TODO: create syntax for Show so i can do User("1", "Oleg").show
    trait Show[T] { // fancy toString
      def show(entity: T): String
    }

    final case class User(id: String, name: String)

    implicit val ShowUser: Show[User] = (user: User) => s"Name: ${user.name} Id: ${user.id}"

    implicit class ShowUser[A](x: A) {
      def show(implicit s: Show[A]): String = {
        s.show(x)
      }
    }

    User("1", "Oleg").show

  }

  object Task3 {

    // TODO: create Parse instance for User
    // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)
    type Error = String
    trait Parse[T] { // invent any format you want or it can be csv string
      def parse(entity: String): Either[Error, T]
    }

    final case class User(id: String, name: String)

    implicit val ParseUser: Parse[User] = (entity: String) => {
      val data: List[String] = entity.split("\\s+").toList
      data match {
        case id :: name :: Nil => Right(User(id, name))
        case _ => Left("Invalid format")
      }
    }

    implicit class ParseOps(x: String) {
      def parse[T](implicit p: Parse[T]): Either[Error, T] = {
        p.parse(x)
      }
    }

  }


  object Task4 {

    // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
    // define the typeclass (think of a method signature)
    // remember `a method b` is `a.method(b)`
    trait Equals[T]{
      def ===(lth: T, rth: T): Boolean
    }

    implicit val EqualsValue: Equals[String] = (lth: String, rth: String) => lth == rth

    implicit class EqualsOps[T](lth: T) {
      def ===(rth: T)(implicit e: Equals[T]): Boolean = {
        e.===(lth, rth)
      }
    }

  }


  object AdvancedHomework {

    // TODO: create a typeclass for flatMap method
    trait FlatMap[F[_]] {
      def flatMap[A, B](x: F[A])(f: A => F[B]): F[B]
    }
  }

}
