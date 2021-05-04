package com.evolution.bootcamp.homework.db.routers

import cats.effect.IO
import com.evolution.bootcamp.homework.db.domain._
import com.evolution.bootcamp.homework.db.services._
import org.http4s._
import org.http4s.dsl.impl.{Root, UUIDVar}
import org.http4s.dsl.io._

object AuthorRouter {

  def of(service: AuthorService[IO]): HttpRoutes[IO] = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO] {

      case GET -> Root / "get" / "author" / UUIDVar(uuid) =>
        service.getAuthor(uuid).flatMap {
          case Left(_) => NotFound()
          case Right(value) => Ok(value)
        }

      case req@POST -> Root / "create" / "author" =>
        for {
          authorP <- req.as[AuthorParams]
          responseE <- service.createAuthor(authorP)
          response <- responseE match {
            case Left(error) => BadRequest(error)
            case Right(value) => Ok(value)
          }
        } yield response

      case DELETE -> Root / "delete" / "author" / UUIDVar(uuid) =>
        service.deleteAuthor(uuid).flatMap {
          case Left(error) => BadRequest(error)
          case Right(value) => Ok(value)
        }
    }
  }

}
