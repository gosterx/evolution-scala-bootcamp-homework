package com.evolution.bootcamp.homework.db.services

import com.evolution.bootcamp.homework.db.domain._
import doobie.{Meta, Read}

import java.time.{LocalDate, Year}
import java.util.UUID

object Mappers {
  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
  implicit val localDateMeta: Meta[LocalDate] = {
    Meta[String].timap(LocalDate.parse)(_.toString)
  }
  implicit val bookRead: Read[Either[BookError, Book]] = Read[(UUID, UUID, String, Year, String)].map{
    case (id, author, title, year, genre) => Book.of(id, author, title, year, genre)
  }
  implicit val authorRead: Read[Either[AuthorError, Author]] = Read[(UUID, String, LocalDate)].map{
    case (id, name, birthday) => Author.of(id, name, birthday)
  }
  implicit val bookWithAuthorRead: Read[BookWithAuthor] = Read[(UUID, UUID, String, LocalDate, String, Year)].map{
    case (id, authorId, name, birthday, title, year) => { for {
      author <- Author.of(authorId, name, birthday)
    } yield BookWithAuthor(id, author, title, year) }.toOption.get
  }
}
