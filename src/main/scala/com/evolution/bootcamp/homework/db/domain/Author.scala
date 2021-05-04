package com.evolution.bootcamp.homework.db.domain

import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

import java.time.LocalDate
import java.util.UUID

sealed trait AuthorError extends Throwable
object AuthorError {
  case object AuthorNotFound extends AuthorError
  case object AuthorValidationError extends AuthorError
}

abstract case class Author private (id: UUID, name: String, birthday: LocalDate)
object Author {
  def of(id: UUID, name: String, birthday: LocalDate): Either[AuthorError, Author] = {
    if (name.nonEmpty) Right(new Author(id, name, birthday) {})
    else               Left(AuthorError.AuthorValidationError)
  }

  implicit val AuthorEncoder: Encoder[Author] =
    Encoder.instance { (author: Author) =>
      Json.obj(
        "id" -> author.id.asJson,
        "name" -> author.name.asJson,
        "birthday" -> author.birthday.asJson
      )
    }
}

@JsonCodec
final case class AuthorParams(name: String, birthday: LocalDate)
