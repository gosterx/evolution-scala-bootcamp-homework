package com.evolution.bootcamp.homework.db.routers

import cats.effect._
import com.evolution.bootcamp.homework.db.domain.BookParams
import com.evolution.bootcamp.homework.db.services.BookService
import io.circe.generic.auto._
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.impl.Root
import org.http4s.dsl.io._

object BookRouter {

  def of(service: BookService[IO]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {

      case GET -> Root / "get" / "book" / UUIDVar(uuid) =>
        service.getBook(uuid).flatMap {
          case Left(_) => NotFound()
          case Right(value) => Ok(value)
        }

      case GET -> Root / "get" / "books" =>
        service.getBooksWithAuthor.flatMap(res => Ok(res))

      case req @ POST -> Root / "create" / "book" =>
        for {
          bookP <- req.as[BookParams]
          responseE <- service.createBook(bookP)
          response <- responseE match {
            case Left(error) => BadRequest(error.toString)
            case Right(value) => Ok(value)
          }
        } yield response

      case req @ POST -> Root / "update" / "book" / UUIDVar(uuid) =>
        for {
          bookP <- req.as[BookParams]
          responseE <- service.updateBook(uuid, bookP)
          response <- responseE match {
            case Left(error) => BadRequest(error.toString)
            case Right(value) => Ok(value)
          }
        } yield response

      case DELETE -> Root / "delete" / "book" / UUIDVar(uuid) =>
        service.deleteBook(uuid).flatMap {
          case Left(error) => BadRequest(error.toString)
          case Right(value) => Ok(value)
        }
    }
  }
}
