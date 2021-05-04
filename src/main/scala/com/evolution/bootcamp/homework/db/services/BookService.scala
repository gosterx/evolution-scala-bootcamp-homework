package com.evolution.bootcamp.homework.db.services

import cats.Monad
import cats.effect._
import cats.syntax.all._
import com.evolution.bootcamp.homework.db.database.InitInfo
import com.evolution.bootcamp.homework.db.domain.BookError._
import com.evolution.bootcamp.homework.db.domain._
import com.evolution.bootcamp.homework.db.effects.GenUUID
import com.evolution.bootcamp.homework.db.services.Mappers._
import doobie._
import doobie.implicits._

import java.util.UUID

trait BookService[F[_]] {
  def getBook(id: UUID): F[Either[BookError ,Book]]
  def getBooksWithAuthor: F[List[BookWithAuthor]]
  def createBook(book: BookParams): F[Either[BookError, Book]]
  def updateBook(id: UUID, book: BookParams): F[Either[BookError, Book]]
  def deleteBook(id: UUID): F[Either[BookError, Book]]
}

object BookService {

  def of[F[_] : Sync](transactor: Transactor[F], authorService: AuthorService[F]): F[BookService[F]] = {

    val initTable: Fragment = Fragment.const(
      """CREATE TABLE books (
        | id UUID PRIMARY KEY,
        | author UUID NOT NULL,
        | title VARCHAR(100) NOT NULL,
        | year INT,
        | genre VARCHAR(100),
        | FOREIGN KEY (author) REFERENCES authors(id));""".stripMargin)

    val insertTable: Fragment = Fragment.const(
      s"""INSERT INTO books(id, author, title, year, genre) VALUES
        | ('${InitInfo.bookScala}', '${InitInfo.authorOdersky}', 'Programming in Scala', 2016, 'Documentation'),
        | ('${InitInfo.bookHPStone}', '${InitInfo.authorRowling}', 'Harry Potter and Philosopher''s Stone', 1997, 'Fantasy'),
        | ('${InitInfo.bookHPSecrets}', '${InitInfo.authorRowling}', 'Harry Potter and the Chamber of Secrets', 1998, 'Fantasy');
        |""".stripMargin)

    def setup: ConnectionIO[Unit] = for {
      _ <- initTable.update.run
      _ <- insertTable.update.run
    } yield ()

    for {
      _ <- setup.transact[F](transactor)
    } yield new BookService[F] {

      override def getBook(id: UUID): F[Either[BookError, Book]] =
        fr"SELECT id, author, title, year, genre FROM books WHERE id = $id"
          .query[Either[BookError, Book]].option.transact(transactor)
          .map{
            case Some(value) => value match {
              case Left(value) => value.asLeft
              case Right(author) => author.asRight
            }
            case None => BookNotFound.asLeft
          }

      override def getBooksWithAuthor: F[List[BookWithAuthor]] =
        fr"SELECT b.id, a.id, a.name, a.birthday, b.title, b.year FROM books b INNER JOIN authors a ON b.author = a.id"
          .query[BookWithAuthor].to[List].transact(transactor)

      override def createBook(bookP: BookParams): F[Either[BookError, Book]] = {
        for {
          author <- authorService.getAuthor(bookP.authorId)
          bookId <- GenUUID[F].random
          bookE  <- author match {
            case Left(_)  => Sync[F].raiseError(BookValidationError)
            case Right(_) => Monad[F].pure(Book.of(bookId, bookP.authorId, bookP.title, bookP.year, bookP.genre))
          }
          book <- bookE  match {
            case Left(_) => Sync[F].raiseError(BookValidationError)
            case Right(value) => Monad[F].pure(value)
          }
          _ <- sql"""INSERT INTO books (id, author, title, year, genre) VALUES ($bookId, ${bookP.authorId}, ${bookP.title}, ${bookP.year}, ${bookP.genre})"""
            .update.run
            .transact(transactor)
        } yield bookE
      }

      override def updateBook(id: UUID, bookP: BookParams): F[Either[BookError, Book]] = {
        for {
          bookE <- getBook(id)
          authorE <- authorService.getAuthor(bookP.authorId)
          book <- bookE match { //Book checking
            case Left(_) => Sync[F].raiseError(BookNotFound)
            case Right(_) => authorE match { //Author checking
              case Left(_) => Sync[F].raiseError(BookValidationError)
              case Right(_) => Book.of(id, bookP.authorId, bookP.title, bookP.year, bookP.genre) match { //Book validation
                case Left(_) => Sync[F].raiseError(BookValidationError)
                case Right(value) => Monad[F].pure(value)
              }
            }
          }
          _ <- sql"UPDATE books SET authorId = ${book.authorId}, title = ${book.title}, year = ${book.year}, genre = ${book.genre} WHERE id = $id"
                .update
                .run
                .transact(transactor)
        } yield bookE
      }

      override def deleteBook(id: UUID): F[Either[BookError, Book]] =
        for {
          bookE <- getBook(id)
          book  <- bookE match {
            case Left(_) => Sync[F].raiseError(BookNotFound)
            case Right(value) => sql"DELETE FROM books WHERE id = $id".update.run.transact(transactor) *> Sync[F].pure(value)
          }
        } yield bookE
    }
  }
}
