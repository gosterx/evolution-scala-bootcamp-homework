package com.evolution.bootcamp.homework.db.domain

import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

import java.time.Year
import java.util.UUID

sealed trait BookError extends Throwable
object BookError {
  case object BookNotFound extends BookError
  case object BookValidationError extends BookError
}

sealed abstract case class Book private(id: UUID, authorId: UUID, title: String, year: Year, genre: String)
object Book {
  def of(id: UUID, authorId: UUID, title: String, year: Year, genre: String): Either[BookError, Book] = {
    if (title.nonEmpty && genre.nonEmpty) Right(new Book(id, authorId, title, year, genre) {})
    else                                  Left(BookError.BookValidationError)
  }

  implicit val BookEncoder: Encoder[Book] =
    Encoder.instance { (book: Book) =>
      Json.obj(
        "id" -> book.id.asJson,
        "authorId" -> book.authorId.asJson,
        "title" -> book.title.asJson,
        "year" -> book.year.asJson,
        "genre" -> book.genre.asJson
      )
    }
}

@JsonCodec
final case class BookParams(authorId: UUID, title: String, year: Year, genre: String)
