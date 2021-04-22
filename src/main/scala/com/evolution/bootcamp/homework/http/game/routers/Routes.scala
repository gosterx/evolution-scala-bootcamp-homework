package com.evolution.bootcamp.homework.http.game.routers

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.evolution.bootcamp.homework.http.game.domain.attempt.Attempt
import com.evolution.bootcamp.homework.http.game.domain.game._
import com.evolution.bootcamp.homework.http.game.effects.{GenNumber, GenUUID}
import io.circe.generic.auto._
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._

import java.util.UUID

object Routes {

  def routes(ref: Ref[IO, Map[UUID, Game]]): HttpRoutes[IO] = HttpRoutes.of[IO]{
    case req @ POST -> Root / "start" =>
      (for {
        newGame <- req.as[GameParams]
        min = newGame.min
        max = newGame.max
        attempts = newGame.attempts
        id      <- GenUUID[IO].random
        number  <- GenNumber[IO].random(min, max)
        _       <- ref.update(_ + (id -> Game(id, min, max, number, attempts)))
      } yield id).flatMap(id => Ok(Started(id)))

    case req @ POST -> Root / "play" =>
      for {
        attempt <- req.as[Attempt]
        id = attempt.id
        guess = attempt.number
        gameOption <- ref.modifyMaybe { games =>
          games.get(id).map { game =>
            val updatedGame = game.copy(attempts = game.attempts - 1)
            (games - id + (id -> updatedGame), updatedGame)
          }
        }
        response <- gameOption.map { game =>
          Ok() *> Ok(
              if (game.attempts == 0) Defeat
              else if (game.number > guess) Greater
              else if (game.number < guess) Lower
              else if (game.number == guess) Win
              else GameNotFound)
        }.get
      } yield response
  }

}
