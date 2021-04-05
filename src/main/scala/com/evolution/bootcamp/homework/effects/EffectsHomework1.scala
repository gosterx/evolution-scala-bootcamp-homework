package com.evolution.bootcamp.homework.effects

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

import scala.concurrent.ExecutionContext.Implicits.global

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1 extends App {

  final class IO[A](private val run: () => A) {
    def map[B](f: A => B): IO[B] = new IO(() => f(run()))
    def flatMap[B](f: A => IO[B]): IO[B] = new IO(() => f(run()).run())
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = map(_ => ())
    def attempt: IO[Either[Throwable, A]] = new IO(() => Try(run()).toEither)
    def option: IO[Option[A]] = new IO(() => Try(run()).toOption)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = new IO(() => Try(run()) match {
      case Success(value) => value
      case Failure(exception) => f(exception).run()
    })
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] =  new IO ( () => Try(run()) match {
      case Success(value) => map(value)
      case Failure(exception) => recover(exception)
    })
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = new IO(() => Try(run()) match {
      case Success(value) => bind(value).run()
      case Failure(exception) => recover(exception).run()
    })
    def unsafeRunSync(): A = run()
    def unsafeToFuture(): Future[A] = Future { run() }
  }

    object IO {
      def apply[A](body: => A): IO[A] = ???
      def suspend[A](thunk: => IO[A]): IO[A] = ???
      def delay[A](body: => A): IO[A] = ???
      def pure[A](a: A): IO[A] = new IO(() => a)
      def fromEither[A](e: Either[Throwable, A]): IO[A] = ???
      def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = ???
      def fromTry[A](t: Try[A]): IO[A] = ???
      def none[A]: IO[Option[A]] = ???
      def raiseError[A](e: Throwable): IO[A] = ???
      def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = ???
      def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = ???
      def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = ???
      def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = ???
      val unit: IO[Unit] = ???
    }

}
