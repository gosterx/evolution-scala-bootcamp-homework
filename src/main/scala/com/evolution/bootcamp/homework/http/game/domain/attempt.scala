package com.evolution.bootcamp.homework.http.game.domain

import io.circe.generic.JsonCodec

import java.util.UUID

object attempt {

  @JsonCodec
  final case class Attempt(id: UUID, number: Int)

}
