package com.evolution.bootcamp.homework.http.game.domain

import io.circe.generic.JsonCodec

import java.util.UUID

object game {

  @JsonCodec
  sealed trait GameResult
  @JsonCodec
  case class Started(id: UUID) extends GameResult
  case object Win extends GameResult
  case object Defeat extends GameResult
  case object Greater extends GameResult
  case object Lower extends GameResult
  case object GameNotFound extends GameResult

  @JsonCodec
  final case class GameParams(min: Int, max: Int, attempts: Int)
  final case class Game(id: UUID, min: Int, max: Int, number: Int, attempts: Int)

}
