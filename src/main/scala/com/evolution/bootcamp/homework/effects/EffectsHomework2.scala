package com.evolution.bootcamp.homework.effects

import cats.MonadError
import cats.effect.concurrent.Ref
import cats.effect.syntax.all._
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
import com.evolution.bootcamp.homework.effects.EffectsHomework2.ValidationError.{InvalidFilePath, SeedIsNotANumber}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.io.{BufferedSource, Source, StdIn}
import scala.util.{Failure, Success, Try}

object EffectsHomework2 extends IOApp{

  sealed trait Console[F[_]]{
    def readStr: F[String]
    def putStr(str: String): F[Unit]
  }
  object Console {
    def of[F[_]: Sync]: Console[F] = new Console[F] {
      override def readStr: F[String] = Sync[F].delay(StdIn.readLine())
      override def putStr(text: String): F[Unit] = Sync[F].delay(println(text))
    }
  }

  sealed trait ValidationError extends Throwable{
    def getMessage: String
  }
  object ValidationError {
    case object InvalidFilePath extends ValidationError {
      override def getMessage: String = "No such file or directory"
    }
    case object SeedIsNotANumber extends ValidationError {
      override def getMessage: String = "Seed must be integer"
    }
  }

  sealed trait HashFunction {
    def function(word: String, seed: Int): Int
    val functionName: String
  }
  object HashFunction {
    def javaHash: HashFunction = new HashFunction {
      override def function(word: String, seed: Int): Int = {
        var hash = 0
        for (ch <- word.toCharArray)
          hash = 31 * hash + ch.toInt
        hash = hash ^ (hash >> 20) ^ (hash >> 12)
        hash ^ (hash >> 7) ^ (hash >> 4)
      }
      override val functionName: String = "javaHash"
    }

    def knuthHash: HashFunction = new HashFunction {
      override def function(word: String, seed: Int): Int = {
        var hash = 0
        for (ch <- word.toCharArray)
          hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
        hash % seed
      }
      override val functionName: String = "knuthHash"
    }
  }

  sealed trait Hashing[F[_]] {
    def getHashSignature(hashFunction: (String, Int) => Int, words: List[String], seed: Int): F[Int]
  }
  object Hashing {
    def of[F[_]: Sync]: Hashing[F] = new Hashing[F] {
      override def getHashSignature(hashFunction: (String, Int) => Int, words: List[String], seed: Int): F[Int] = {
        Sync[F].delay(words.map(word => hashFunction(word, seed)).min)
      }
    }
  }

  sealed trait Storage[F[_]]{
    def update(key: String, hash: Int): F[Unit]
    def get: F[Map[String, Int]]
  }

  case class HashStorage[F[_]: Sync]() extends Storage[F] {
    lazy val storageMap: Ref[F, Map[String, Int]] = Ref.unsafe(Map.empty)
    val storage: F[Ref[F, Map[String, Int]]] = Sync[F].delay(storageMap)

    override def update(key: String, hash: Int): F[Unit] =
      for {
        st <- storage
        _  <- st.update(prev => prev + (key -> hash))
      } yield ()


    override def get: F[Map[String, Int]] = for {
      st  <- storage
      map <- st.get
    } yield map
  }

  def readFile[F[_]: Sync](blocker: Blocker)(implicit contextShift: ContextShift[F], console: Console[F]): F[List[String]] = {
    val filePath: F[String] = blocker.blockOn[F, String] {
      console.putStr("Enter the filepath:") *>
      console.readStr
    }

    def fileData(filePath: String): F[List[String]] =
      (Try(Source.fromFile(filePath)) match {
        case Success(value) => Sync[F].delay(value)
        case Failure(_) => MonadError[F, Throwable].raiseError[BufferedSource](InvalidFilePath)
      }).bracket {
          descriptor =>
            Sync[F].delay(descriptor.getLines().toList)
        } { source =>
          Sync[F].delay(source.close())
        }.handleErrorWith(error =>
          console.putStr(error.getMessage) *>
          readFile(blocker))

    for {
      path <- filePath
      data <- fileData(path)
    } yield data
  }

  def readSeed[F[_]: Sync](blocker: Blocker)(implicit contextShift: ContextShift[F], console: Console[F]): F[Int] = {
    blocker.blockOn{
      for {
        _ <- console.putStr("Enter the seed:")
        str <- console.readStr
        seed <- Try(str.toInt) match {
          case Success(value) => Sync[F].delay(value)
          case Failure(_) => MonadError[F, Throwable].raiseError[Int](SeedIsNotANumber)
        }
      } yield seed
    }.handleErrorWith(error => console.putStr(error.getMessage) *> readSeed(blocker))
  }

  def splitText[F[_]: Sync](text: List[String]): F[List[String]] = {
    Sync[F].delay(
      text.flatMap(str => str.split(' ').toList.map(_.filterNot(str =>
          str == ',' ||
          str == '!' ||
          str == '?' ||
          str == '.' ||
          str.isSpaceChar))))
  }

  override def run(args: List[String]): IO[ExitCode] = {

    val executor = Executors.newFixedThreadPool(4)
    val ex = ExecutionContext.fromExecutor(executor)
    val blocker = Blocker.liftExecutionContext(ex)

    implicit val console: Console[IO] = Console.of[IO]

    val hashing: Hashing[IO] = Hashing.of[IO]
    val hashInfoStorage: HashStorage[IO] = HashStorage[IO]()

    for {
      data                <- readFile[IO](blocker)
      seed                <- readSeed[IO](blocker)
      words               <- splitText[IO](data)
      javaHashFiber       <- hashing.getHashSignature(HashFunction.javaHash.function, words, seed).start
      knuthHashFiber      <- hashing.getHashSignature(HashFunction.knuthHash.function, words, seed).start
      javaHashSignature   <- javaHashFiber.join
      knuthHashSignature  <- knuthHashFiber.join
      _                   <- hashInfoStorage.update(HashFunction.javaHash.functionName, javaHashSignature)
      _                   <- hashInfoStorage.update(HashFunction.knuthHash.functionName, knuthHashSignature)
      storage             <- hashInfoStorage.get
      _                   <- IO(println(storage))
    } yield ExitCode.Success
  }
}

