package com.evolution.bootcamp.homework.http.game

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.effect.Resource._
import cats.syntax.all._
import com.evolution.bootcamp.homework.http.game.domain.attempt.Attempt
import com.evolution.bootcamp.homework.http.game.domain.game._
import com.evolution.bootcamp.homework.http.game.effects.{Console, GenUUID}
import io.circe.Json
import io.circe.parser._
import io.circe.syntax.EncoderOps
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax

import java.net.http.HttpClient
import java.util.UUID

object WsGuessClient extends IOApp{

  private val uri = uri"ws://localhost:9002/wsgame"

  def process(id: UUID, min: Int, max: Int, client: WSConnectionHighLevel[IO]): IO[Unit] = {
    val guess = min + (max - min) / 2
    for {
      _ <- client.send(WSFrame.Text(Attempt(id, guess).asJson.spaces2))
      wsFrameOpt <- client.receive
      status = wsFrameOpt.get match {
        case WSFrame.Text(data, _) => decode[GameResult](data)
        case _ => Left(new Throwable("error"))
      }
      _ <- status match {
        case Right(Win) => Console[IO].printLine(s"You WON!!! Number was $guess")
        case Right(Defeat) => Console[IO].printLine(s"over!")
        case Right(Greater) => Console[IO].printLine(s"The planned number is greater than $guess") *> process(id, guess, max, client)
        case Right(Lower) => Console[IO].printLine(s"The planned number is lower than $guess") *> process(id, min, guess, client)
        case Right(GameNotFound) => Console[IO].printLine("Game not found!")
      }
    } yield ()

  }

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource.eval(IO(HttpClient.newHttpClient))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use { client =>
      for {
        _ <- Console[IO].printLine("Game starting...")
        _ <- client.send(WSFrame.Text(GameParams(10, 20, 10).asJson.spaces2))
        wsFrameOpt <- client.receive
        result  <-  IO(wsFrameOpt.get match {
          case WSFrame.Text(data, _) => decode[GameResult](data)
        })
        id <- result match {
          case Right(gameResult) => IO(gameResult.asInstanceOf[Started].id)
          case Left(error) => IO.raiseError(error)
        }
        _ <- Console[IO].printLine(s"Game started with id $id")
        _ <- process(id, 10, 20, client)
      } yield ExitCode.Success
    }
  }

}
