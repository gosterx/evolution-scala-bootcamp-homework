package com.evolution.bootcamp.homework.db.services

import cats.Monad
import cats.effect._
import cats.syntax.all._
import com.evolution.bootcamp.homework.db.database.InitInfo
import com.evolution.bootcamp.homework.db.domain.AuthorError._
import com.evolution.bootcamp.homework.db.domain._
import com.evolution.bootcamp.homework.db.effects.GenUUID
import com.evolution.bootcamp.homework.db.services.Mappers._
import doobie._
import doobie.implicits._

import java.util.UUID

trait AuthorService[F[_]]{
  def getAuthor(id: UUID): F[Either[AuthorError, Author]]
  def createAuthor(author: AuthorParams): F[Either[AuthorError, Author]]
  def deleteAuthor(id: UUID): F[Either[AuthorError, Author]]
}

object AuthorService {

  def of[F[_]: Sync](transactor: Transactor[F]): F[AuthorService[F]] = {


    val initTable: Fragment = Fragment.const(
    """CREATE TABLE authors (
      | id UUID PRIMARY KEY,
      | name VARCHAR(100) NOT NULL,
      | birthday DATE);""".stripMargin)

    val insertTable: Fragment = Fragment.const(
    s"""INSERT INTO authors(id, name, birthday) VALUES
      | ('${InitInfo.authorOdersky}', 'Martin Odersky', '1958-09-05'),
      | ('${InitInfo.authorRowling}', 'J.K. Rowling', '1965-07-31');
      |""".stripMargin)

    def setup: ConnectionIO[Unit] = for {
      _ <- initTable.update.run
      _ <- insertTable.update.run
    } yield ()

    for {
      _ <- setup.transact[F](transactor)
    } yield new AuthorService[F] {

      override def getAuthor(id: UUID): F[Either[AuthorError, Author]] =
        fr"SELECT id, name, birthday FROM authors WHERE id = $id"
          .query[Either[AuthorError, Author]].option.transact(transactor)
          .map{
            case Some(value) => value match {
              case Left(value) => value.asLeft
              case Right(author) => author.asRight
            }
            case None => AuthorNotFound.asLeft
          }

      override def createAuthor(authorP: AuthorParams): F[Either[AuthorError, Author]] =
        for {
          authorId <- GenUUID[F].random
          authorE  <- Sync[F].pure(Author.of(authorId, authorP.name, authorP.birthday))
          author   <- authorE match {
            case Left(value) => Sync[F].raiseError(value)
            case Right(value) => Monad[F].pure(value)
          }
          _ <- sql"INSERT INTO authors (id, name, birthday) VALUES ($authorId, ${author.name}, ${author.birthday})"
                .update.run
                .transact(transactor)
        } yield authorE

      override def deleteAuthor(id: UUID): F[Either[AuthorError, Author]] =
        for {
          authorE <- getAuthor(id)
          author  <- authorE match {
            case Left(_) => Sync[F].raiseError(AuthorNotFound)
            case Right(value) => sql"DELETE FROM authors WHERE id = $id".update.run.transact(transactor) *> Sync[F].pure(value)
          }
        } yield authorE
    }
  }
}

