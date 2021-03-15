package com.evolution.bootcamp.homework.basics

import java.lang.Math.abs
import scala.annotation.tailrec

object Basics extends App {
  def lcm(a: Int, b: Int): Option[Int] = {
    if (a == 0 && b == 0) None
    else Some(abs(a * b) / gcd(a, b))
  }

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) abs(a) else gcd(abs(b), abs(a) % abs(b))

}
