package com.evolution.bootcamp.homework.http.game.effects

import cats.effect.Sync

import scala.util.Random

trait GenNumber[F[_]] {
  def random(min: Int, max: Int): F[Int]
}

object GenNumber {

  def apply[F[_]: GenNumber]: GenNumber[F] = implicitly

  implicit def of[F[_]: Sync]: GenNumber[F] = new GenNumber[F] {
    override def random(min: Int, max: Int): F[Int] =
      Sync[F].delay(min + Random.nextInt(max - min))
  }
}
