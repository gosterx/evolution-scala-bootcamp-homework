package com.evolution.bootcamp.homework.db

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import com.evolution.bootcamp.homework.db.database.DBTransactor
import com.evolution.bootcamp.homework.db.routers.{AuthorRouter, BookRouter}
import com.evolution.bootcamp.homework.db.services._
import org.http4s._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

object BookServiceServer extends IOApp{

  def routes(bookService: BookService[IO], authorService: AuthorService[IO]): HttpApp[IO] = {
    BookRouter.of(bookService) <+> AuthorRouter.of(authorService)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    DBTransactor.make[IO]
      .use{ tx =>
        for {
          authorService <- AuthorService.of[IO](tx)
          bookService   <- BookService.of[IO](tx, authorService)
          _             <- httpServer(routes(bookService, authorService))
        } yield ()
      }.as(ExitCode.Success)
  }

  def httpServer(routes: HttpApp[IO]): IO[Unit] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(routes)
      .serve
      .compile
      .drain

}
