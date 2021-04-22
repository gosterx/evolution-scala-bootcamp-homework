package com.evolution.bootcamp.homework.http.game.effects

import cats.effect.Sync

trait Console[F[_]]{
  def printLine(value: String): F[Unit]
}

object Console {

  def apply[F[_]: Console]: Console[F] = implicitly

  implicit def of[F[_]: Sync]: Console[F] = new Console[F] {
    override def printLine(value: String): F[Unit] = Sync[F].delay(println(value))
  }
}
