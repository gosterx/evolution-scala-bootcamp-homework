package com.evolution.bootcamp.homework.http.game

import cats.effect.IO.ioEffect
import cats.effect.{ExitCode, IO, IOApp}
import com.evolution.bootcamp.homework.http.game.domain.attempt.Attempt
import com.evolution.bootcamp.homework.http.game.domain.game.{GameResult, _}
import com.evolution.bootcamp.homework.http.game.effects._
import io.circe.generic.auto.exportDecoder
import org.http4s.Method
import org.http4s.circe.CirceEntityCodec._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.implicits.http4sLiteralsSyntax

import java.util.UUID
import scala.concurrent.ExecutionContext

object GuessClient extends IOApp{

  private val uri = uri"http://localhost:9002"

  override def run(args: List[String]): IO[ExitCode] = {

    def process(id: UUID, min: Int, max: Int, client: Client[IO]): IO[Unit] = {
      val guess = min + (max - min) / 2
      client.expect[GameResult](Method.POST(Attempt(id, guess), uri / "play")).flatMap {
        case Win => Console[IO].printLine(s"You WON!!! Number was $guess")
        case Defeat => Console[IO].printLine(s"over!")
        case Greater => Console[IO].printLine(s"The planned number is greater than $guess") *> process(id, guess, max, client)
        case Lower => Console[IO].printLine(s"The planned number is lower than $guess") *> process(id, min, guess, client)
        case GameNotFound => Console[IO].printLine("Game not found!")
      }
    }

    BlazeClientBuilder[IO](ExecutionContext.global).resource.use{ client =>
      for {
        _ <- Console[IO].printLine("Game starting...")
        res <- client.expect[Started](Method.POST(GameParams(10, 20, 10), uri / "start"))
        id = res.id
        _ <- Console[IO].printLine(s"Game started with id $id")
        _ <- process(id, 10, 20, client)
      } yield ExitCode.Success
    }
  }

}
