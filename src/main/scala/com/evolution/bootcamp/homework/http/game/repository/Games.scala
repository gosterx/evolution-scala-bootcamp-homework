package com.evolution.bootcamp.homework.http.game.repository

import com.evolution.bootcamp.homework.http.game.domain.game.Game
import com.evolution.bootcamp.homework.http.game.repository.Games.ValidationError

import java.util.UUID

trait Games[F[_]]{
  def all: F[Map[UUID, Game]]
  def create(id: UUID, min: Int, max: Int, number: Int, attempts: Int): F[Either[ValidationError, Game]]

}

object Games {

  sealed trait ValidationError extends Throwable
}
