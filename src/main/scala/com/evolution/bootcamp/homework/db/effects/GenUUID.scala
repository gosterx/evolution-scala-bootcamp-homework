package com.evolution.bootcamp.homework.db.effects

import cats.effect.Sync

import java.util.UUID

trait GenUUID[F[_]] {
  def random: F[UUID]
  def read(value: String): F[UUID]
}

object GenUUID {

  def apply[F[_]: GenUUID]: GenUUID[F] = implicitly

  implicit def of[F[_]: Sync]: GenUUID[F] = new GenUUID[F] {
    override def random: F[UUID] = Sync[F].delay(UUID.randomUUID())
    override def read(value: String): F[UUID] = Sync[F].delay(UUID.fromString(value))
  }
}
