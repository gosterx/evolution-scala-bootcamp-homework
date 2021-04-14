package com.evolution.bootcamp.homework.effects

import cats.{Applicative, Monad}
import cats.syntax.all._
import cats.effect.concurrent.Ref
import cats.effect.{ Clock, Concurrent, ExitCode, IO, IOApp, Timer}

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */

object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]

    def checkOnExpirationTimestamp(): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
                                              state: Ref[F, Map[K, (Long, V)]],
                                              expiresIn: FiniteDuration
                                            ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = {
      state.get.map(stateMap =>
        stateMap
          .find { case (k, _) => k == key}
          .map { case (_, (_, v)) => v} )
    }

    def put(key: K, value: V): F[Unit] = for {
      timeOfInsertion <- Clock[F].realTime(MILLISECONDS)
      _               <- state.update(st => st.updated(key, (timeOfInsertion + expiresIn.toMillis, value)))
    } yield ()


    def checkOnExpirationTimestamp(): F[Unit] = {
      for {
        now <- Clock[F].realTime(MILLISECONDS)
        _    <- state.update(_.filter{ case (_, (expirationTimestamp, _ )) => now >= expirationTimestamp })
      } yield ()
    }

  }

  object ExpirationChecker {
    def startCheckerLoop[F[_] : Monad : Clock, K, V](cache: RefCache[F, K, V], checkOnExpirationsEvery: FiniteDuration)
                                                    (implicit T: Timer[F]): F[Unit] = {
      for {
        _ <- T.sleep(checkOnExpirationsEvery)
        _ <- cache.checkOnExpirationTimestamp()
        _ <- startCheckerLoop(cache, checkOnExpirationsEvery)
      } yield()
    }
  }

  object Cache {
    def of[F[_] : Clock, K, V](
                                expiresIn: FiniteDuration,
                                checkOnExpirationsEvery: FiniteDuration
                              )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      for {
        state <- Ref.of(Map.empty[K, (Long, V)])
        cache <- Applicative[F].pure(new RefCache(state, expiresIn))
        _     <- Concurrent[F].start(ExpirationChecker.startCheckerLoop(cache, checkOnExpirationsEvery))
      } yield cache
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}

