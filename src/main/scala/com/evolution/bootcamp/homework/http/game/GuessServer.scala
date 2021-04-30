package com.evolution.bootcamp.homework.http.game

import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import com.evolution.bootcamp.homework.http.game.domain.game._
import com.evolution.bootcamp.homework.http.game.routers.Routes.{routes, socketRoutes}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{Request, Response}

import java.util.UUID
import scala.concurrent.ExecutionContext

object GuessServer extends IOApp{

  def httpApp(ref: Ref[IO, Map[UUID, Game]]): Kleisli[IO, Request[IO], Response[IO]] = socketRoutes(ref).orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    Ref.of[IO, Map[UUID, Game]](Map.empty).flatMap{ ref =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9002, host = "localhost")
        .withHttpApp(httpApp(ref))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }

}
