package com.evolution.bootcamp.homework.http.game.routers

import cats.effect.IO.ioConcurrentEffect
import cats.effect._
import cats.effect.concurrent.Ref
import com.evolution.bootcamp.homework.http.game.domain.attempt.Attempt
import com.evolution.bootcamp.homework.http.game.domain.game._
import com.evolution.bootcamp.homework.http.game.effects.{GenNumber, GenUUID}
import fs2.{Pipe, Pull, Stream}
import fs2.concurrent.Queue
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax.EncoderOps
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.ExecutionContext.global
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
        gameOption <- modifyRef(ref, id)
        response <- gameOption.map { game =>
              if (game.attempts == 0)         Ok(Defeat)
              else if (game.number > guess)   Ok(Greater)
              else if (game.number < guess)   Ok(Lower)
              else if (game.number == guess)  Ok(Win)
              else                            Ok(GameNotFound)
        }.get
      } yield response
  }

  def socketRoutes(ref: Ref[IO, Map[UUID, Game]]): HttpRoutes[IO] = HttpRoutes.of[IO]{

    case GET -> Root / "wsgame" =>

      val receivePipe: Pipe[IO, WebSocketFrame, IO[GameResult]] =
        _.pull.uncons1.flatMap{
          case Some((startFrame, attemptFrames)) => {
            val started: IO[GameResult] = startFrame match {
              case WebSocketFrame.Text(gameParamsString, _) =>
                for {
                  newGame <- IO.fromEither(decode[GameParams](gameParamsString))
                  min = newGame.min
                  max = newGame.max
                  attempts = newGame.attempts
                  id <- GenUUID[IO].random
                  number <- GenNumber[IO].random(min, max)
                  _ <- ref.update(_ + (id -> Game(id, min, max, number, attempts)))
                } yield Started(id).asInstanceOf[GameResult]
            }
            val attempts: Stream[IO, IO[GameResult]] = attemptFrames.map {
              case WebSocketFrame.Text(attemptString, _) =>
                for {
                  attempt <- IO.fromEither(decode[Attempt](attemptString))
                  id = attempt.id
                  guess = attempt.number
                  gameOption <- modifyRef(ref, id)
                  response <- gameOption.map { game =>
                    IO(
                      if (game.attempts == 0)         Defeat.asInstanceOf[GameResult]
                      else if (game.number > guess)   Greater.asInstanceOf[GameResult]
                      else if (game.number < guess)   Lower.asInstanceOf[GameResult]
                      else if (game.number == guess)  Win.asInstanceOf[GameResult]
                      else                            GameNotFound.asInstanceOf[GameResult]
                    )
                  }.get
                } yield response
            }
            Stream(started) ++ attempts
          }.pull.echo
          case None => Pull.done
        }.stream

      val sendPipe: Pipe[IO, IO[GameResult], WebSocketFrame] =
        _.evalMap(_.map(res => WebSocketFrame.Text(res.asJson.spaces2)))

      for {
        queue <- Queue.unbounded[IO, IO[GameResult]](ioConcurrentEffect(IO.contextShift(global)))
        response <- WebSocketBuilder[IO].build(
          receive = queue.enqueue.compose[Stream[IO, WebSocketFrame]](receivePipe),
          send = queue.dequeue.through(sendPipe)
        )
      } yield response
  }

  def modifyRef(ref: Ref[IO, Map[UUID, Game]], id: UUID): IO[Option[Game]] =
    ref.modifyMaybe { games =>
      games.get(id).map { game =>
        val updatedGame = game.copy(attempts = game.attempts - 1)
        (games - id + (id -> updatedGame), updatedGame)
      }
    }
}