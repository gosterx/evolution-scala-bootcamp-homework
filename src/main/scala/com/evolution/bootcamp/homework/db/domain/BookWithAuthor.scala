package com.evolution.bootcamp.homework.db.domain

import java.time.Year
import java.util.UUID

final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year) {
  override def toString: String = s"$title ($year) by ${author.name}"
}
